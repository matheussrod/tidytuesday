
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 34)
english_monarchs_marriages_df <- janitor::clean_names(tuesdata$english_monarchs_marriages_df)

# Analysis ------------------------------------------------------------------------------------
df_plot <- english_monarchs_marriages_df |> 
  dplyr::mutate(
    dplyr::across(dplyr::ends_with('age'), as.integer),
    year_of_marriage = as.integer(year_of_marriage)
  ) |> 
  dplyr::filter(year_of_marriage >= 1100) |> 
  dplyr::select(dplyr::ends_with('age'), year = year_of_marriage)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Almendra SC', 'almendra')
sysfonts::font_add_google('Caudex', 'caudex')
f1 <- 'almendra'
f2 <- 'caudex'

p1 <- ggplot(df_plot) +
  
  ## line between ages ------------------------------------------------------------------------
  geom_segment(
    aes(
      x = king_age,
      xend = consort_age,
      y = year,
      yend = year,
    ),
    color = '#40010d',
  ) +
  
  ## points -----------------------------------------------------------------------------------
  ### king ------------------------------------------------------------------------------------
  geom_point(
    aes(
      x = king_age,
      y = year,
    ),
    color = '#d99e30',
    size = 2,
  ) +
  
  ### consort ---------------------------------------------------------------------------------
  geom_point(
    aes(
      x = consort_age,
      y = year,
    ),
    color = '#a6173d',
    size = 2,
  ) +

  ## scales -----------------------------------------------------------------------------------
  scale_y_reverse(
    breaks = seq(1000, 1950, 100),
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 60, 5),
    position = 'top',
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'English Monarchs and their marriages',
    x = 'Age',
    y = 'Historical year of marriage',
    subtitle = paste0(
      "All monarchs marriages from 1000 to 1950. The <span style='color:#d99e30;'>yellow dots represents",
      " ruler's age</span> <br>and <span style='color:#a6173d;'>red represents consort's age</span>."
    ),
    caption = 'Source: ianVisits · Graphic: Matheus S. Rodrigues',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(
         family = f1,
    ),
    plot.margin = margin(rep(20, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f2,
         size = 22,
         color = '#40010d',
         face = 'bold',
    ),
    plot.subtitle = ggtext::element_markdown(
         family = f1,
         size = 11,
    ),
    plot.caption = element_text(
         color = 'grey40',
         size = 8,
         hjust = 1,
    ),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
          color = 'grey85',
          linetype = 2,
    ),
    axis.ticks.x = element_blank(),
    axis.title = element_text(
         size = 9,
    )
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_34/plot.png'),
  plot = p1,
  width = 6,
  height = 8,
  dpi = 300,
)
