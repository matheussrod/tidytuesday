
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 33)
worlds_fairs <- janitor::clean_names(tuesdata$worlds_fairs)

# Analysis ------------------------------------------------------------------------------------
fairs_duration <- worlds_fairs |> 
  dplyr::select(start_month:end_year) |> 
  dplyr::mutate(duration = (end_year - start_year) * 12 + (end_month - start_month)) |> 
  dplyr::group_by(month = start_month) |> 
  dplyr::summarise(duration = mean(duration)) |> 
  dplyr::right_join(data.frame(month = 1:12), by = 'month') |> 
  dplyr::mutate(duration = ifelse(is.na(duration), 0, duration)) |> 
  dplyr::arrange(month)

df_plot <- fairs_duration |> 
  dplyr::mutate(
    xmin = seq(1, 12, 1),
    xmax = seq(1.75, 12.75, 1),
    ymin = 0,
    ymax = 1,
    fill = duration,
    xtext = (xmin + xmax)/2,
    ytext = ymax + 0.2,
    label = round(duration, 1),
    ymonth = -0.2,
    month_abb = month.abb
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(df_plot) +
  
  ## rects ------------------------------------------------------------------------------------
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
    ),
    color = 'black',
    show.legend = FALSE,
  ) +
  
  ## text -------------------------------------------------------------------------------------
  ### duration --------------------------------------------------------------------------------
  geom_text(
    aes(
      x = xtext,
      y = ytext,
      label = label,
    ),
    family = f2,
    size = 3,
    color = '#283250',
    lineheight = 0.8,
    vjust = 0,
  ) +
  
  ### months ----------------------------------------------------------------------------------
  geom_text(
    aes(
      x = xtext,
      y = ymonth,
      label = month_abb,
    ),
    family = f2,
    color = 'grey40',
    size = 3,
    vjust = 1,
  ) +
  
  ### labels ----------------------------------------------------------------------------------
  geom_text(
    data = data.frame(
      x = -0.5,
      y = c(-0.2, 1.2),
      label = c('Months', 'Duration\n(in months)')
    ),
    aes(
      x = x,
      y = y,
      label = label,
    ),
    family = f2,
    size = 2.5,
    color = 'grey60',
    lineheight = 0.75,
    vjust = 0.5,
    hjust = 0,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_fill_gradient(
    low = '#d0d9db',
    high = '#283250',
  ) +
  
  coord_equal() + 
  ylim(c(-1, 1.75)) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'World\'s Fairs length',
    subtitle = paste0(
      'How the length of a Fair depend on the month in which the fair begins\n',
      ''
    ),
    caption = 'Source: Wikipedia · Graphic: Matheus S. Rodrigues',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme_void() +
  theme(
    text = element_text(
         family = f2,
    ),
    plot.background = element_rect(
         fill = 'white',
         color = NA,
    ),
    plot.margin = margin(rep(20, 4)),
    plot.title = element_text(
         family = f1,
         size = 18,
         hjust = 0.5,
         face = 'bold',
    ),
    plot.subtitle = element_text(
         size = 11,
         hjust = 0.5,
    ),
    plot.caption = element_text(
         hjust = 0.5,
         color = 'grey50',
         size = 8,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_33/plot.png'),
  plot = p1,
  width = 7,
  height = 3.5,
  dpi = 300,
)
