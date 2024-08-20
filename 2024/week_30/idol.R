
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 30)
ratings <- janitor::clean_names(tuesdata$ratings)

# Analysis ------------------------------------------------------------------------------------

viewers <- ratings |> 
  dplyr::select(season, show_number, viewers_in_millions) |> 
  dplyr::group_by(season) |> 
  dplyr::reframe(viewers_sum = sum(viewers_in_millions, na.rm = TRUE))

rect_offset <- 70
waterfall <- viewers |> 
  dplyr::mutate(
    viewers_lag = dplyr::lag(viewers_sum),
    diff = viewers_sum - viewers_lag,
    diff = ifelse(is.na(diff), viewers_sum, diff)
  )

df_plot <- waterfall |> 
  dplyr::mutate(
    xmin = season * 100 - 100,
    xmax = xmin + rect_offset,
    ymin = ifelse(season == 1, 0, viewers_lag),
    ymax = viewers_sum,
    fill = dplyr::case_when(
      diff < 0 ~ '#c3202f',
      diff > 0 ~ '#263252',
      .default = 'grey40'
    ),
    xlabel = dplyr::lag((xmin + xmax) / 2),
    ytext = ymin,
    ylabel = ifelse(
      diff > 0, 
      paste0('+ ', round(diff, 0)),
      paste0(round(diff, 0))
    ),
    vjust = ifelse(diff < 0, 1.5, -0.5),
    vjust = ifelse(dplyr::lag(diff) > 0, -0.5, vjust),
    xl = xmin,
    xlend = dplyr::lead(xmax)
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
  
  ## line -------------------------------------------------------------------------------------
  geom_segment(
    aes(
      x = xl,
      xend = xlend,
      y = ymax,
      yend = ymax,
    ),
    color = 'grey85',
  ) +
  
  ## rect -------------------------------------------------------------------------------------
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
    ),
  ) +
  
  ## text -------------------------------------------------------------------------------------
  geom_text(
    aes(
      x = xlabel,
      y = ytext,
      label = ylabel,
      vjust = vjust,
    ),
    hjust = 0.5,
    size = 2.75,
  ) +
  
  ## annotates --------------------------------------------------------------------------------
  annotate(
    'segment',
    x = mean(df_plot$xlabel[6:7]),
    xend = mean(df_plot$xlabel[6:7]),
    y = 0,
    yend = df_plot$ymax[6],
    color = 'grey50',
    linetype = 2,
  ) +
  
  annotate(
    'text',
    x = mean(df_plot$xlabel[3:4]),
    y = 750,
    label = paste0(
      'From season 1 to season 5,\n',
      'it\'s possible to observe a\n',
      'growth of 306% in views,\n',
      'reaching a peak of ',
      max(df_plot$ymax),
      '\nmillion views in season 5.'
    ),
    hjust = 0,
    vjust = 1,
    size = 2.5,
  ) +
  
  annotate(
    'text',
    x = mean(df_plot$xlabel[6:8]),
    y = 750,
    label = paste0(
      'On the other hand, from season 6 onwards,\n',
      'there was a drastic reduction in views,\n',
      'reaching a smaller audience than in\n',
      'first season.'
    ),
    hjust = 0,
    vjust = 1,
    size = 2.5,
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'The Fall of American Idol',
    subtitle = paste0(
      'Across the year, the American Idol viewers has decreased significantly. The audience has increased until season 5.\n',
      'After, reduction was observed in all seasons.*'
    ),
    caption = paste0(
      'Source: Wikipedia · Graphic: Matheus S. Rodrigues\n',
      '*There is no information regarding season 14'
    ),
    x = 'Season',
    y = 'Total number of viewers (in millions)',
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_x_continuous(
    breaks = c(0, seq(40, max(df_plot$xmax), 100)),
    labels = c('', seq(1, 18, 1)),
  ) +
  
  scale_fill_identity() +

  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(20, 4)),
    plot.title = element_text(
         family = f1,
         size = 20,
         face = 'bold',
    ),
    plot.subtitle = element_text(
         size = 10,
         margin = margin(b = 15),
    ),
    plot.caption = element_text(
         size = 7.5,
         color = 'grey40',
         hjust = 0,
    ),
    panel.background = element_blank(),
    axis.text = element_text(
         size = 8,
         color = 'grey30',
    ),
    axis.title = element_text(
         size = 10,
         color = 'grey30',
    ),
    axis.ticks.x = element_blank(),
  )


showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_30/plot.png'),
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300,
)
