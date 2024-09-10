
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 32)
olympics <- janitor::clean_names(tuesdata$olympics)

# Analysis ------------------------------------------------------------------------------------
bra_medals <- olympics |> 
  dplyr::filter(
    noc == 'BRA',
    season == 'Summer',
    !is.na(medal)
  ) |>
  dplyr::count(sport, medal) |> 
  dplyr::mutate(
    tot = sum(n),
    .by = sport
  )

df_plot <-
  bra_medals |> 
  dplyr::mutate(
    fill = dplyr::case_match(medal,
      'Bronze' ~ 'darkorange',
      'Silver' ~ 'grey80',
      'Gold' ~ 'gold'
    )
  ) |> 
  dplyr::mutate(medal = ordered(medal, c('Gold', 'Silver', 'Bronze')))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
f1 <- 'outfit'
f2 <- 'noto'


p1 <- ggplot(df_plot) +
  ## col --------------------------------------------------------------------------------------
  geom_col(
    aes(
      y = forcats::fct_reorder(sport, tot),
      x = n,
      fill = medal,
    ),
    color = 'black',
    position = 'stack',
  ) +
  
  ## scale ------------------------------------------------------------------------------------
  scale_fill_manual(
    values = c(
      'Bronze' = '#bF5e3b',
      'Silver' = '#babdbf',
      'Gold' = '#f2da63'
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 150, 25),
    limits = c(0, 150),
    expand = c(0, 2),
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'Brazil\'s Olympics Medals',
    subtitle = paste0(
      'Number of medals won by Brazil in each sport from 1896 to 2016.\n',
      'Each athlete\'s medal was considered to calculate the number of\nmedals.'
    ),
    y = NULL,
    x = 'Quantity',
    caption = 'Source: Sports Reference · Graphic: Matheus S. Rodrigues',
    fill = 'Medal',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(20, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f1,
         size = 18,
         face = 'bold',
    ),
    plot.subtitle = element_text(
         size = 10,
         margin = margin(b = 40),
    ),
    plot.caption = element_text(
         hjust = 1,
         color = 'grey40',
         margin = margin(t = 10),
    ),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
          color = 'grey85',
    ),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
         size = 9
    ),
    axis.text.x = element_text(
         size = 8,
    ),
    legend.position = c(0, 1.06),
    legend.direction = 'horizontal',
    legend.title = element_text(
           size = 9,
    ),
    legend.text = element_text(
           size = 8,
    ),
    legend.key.size = grid::unit(15, 'pt'),
  )


showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_32/plot.png'),
  plot = p1,
  width = 5,
  height = 7,
  dpi = 300,
)
