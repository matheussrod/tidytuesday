
# US Egg Production -------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 15)
egg_production <- tuesdata$`egg-production`
cage_free_percentages <- tuesdata$`cage-free-percentages`

year_prod <- egg_production |> 
  dplyr::select( -source) |> 
  dplyr::filter(!(prod_type == 'table eggs' & prod_process == 'all')) |>
  dplyr::mutate(year = lubridate::year(observed_month)) |> 
  dplyr::group_by(year, prod_type) |> 
  dplyr::summarise(n_eggs = sum(n_eggs, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(names_from = prod_type, values_from = n_eggs) |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    prop = table_eggs / hatching_eggs,
    y_hatching = 0,
    y_table = y_hatching + prop
  )

year_offset <- 0.15

df_bezier <- year_prod |> 
  dplyr::select(year) |> 
  dplyr::mutate(
    offset = list(
      c(-rep(year_offset, 2), rep(year_offset, 2))
    )
  ) |> 
  tidyr::unnest(offset) |> 
  dplyr::mutate(
    x = year + offset,
    y = rep(c(0, -0.3, -0.3, 0), length(year_prod$year)),
    type = 'quadratic'
  ) |> 
  dplyr::left_join(year_prod[, c('year', 'prop')], by = 'year')

df_path <- df_bezier |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(x_min = min(x), x_max = max(x)) |> 
  dplyr::ungroup() |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    x_path = list(seq(x_min, x_max, by = 0.025)),
    y = list(c(0, -0.04, -0.045, -0.025, -0.04, -0.035, -0.0375,
          -0.025, -0.03, -0.02, -0.04, -0.025, 0))
  ) |> 
  tidyr::unnest(c(x_path, y)) |> 
  dplyr::left_join(year_prod[, c('year', 'prop')], by = 'year')


library(ggplot2)
library(ggtext)
library(showtext)

showtext_auto()
font_add_google('Noto Sans', 'noto')
font_add_google('Bangers', 'bangers')
f1 <- 'bangers'
f2 <- 'noto'

showtext_opts(dpi = 100)
p1 <- ggplot(year_prod) + 
  ggforce::geom_bezier(
    data = df_bezier,
    aes(x = x, y = y, linetype = type, group = year),
    color = '#F29877', size = 0.75, show.legend = FALSE
  ) +
  geom_path(
    data = df_path,
    aes(x = x_path, y = y, group = year), size = 0.75, color = '#F29877'
  ) +
  geom_path(
    data = df_path,
    aes(x = x_path, y = y * 2 + prop, group = year), size = 0.75, color = '#8C3D35'
  ) +
  ggforce::geom_bezier(
    data = df_bezier,
    aes(x = x, y = -y + prop, linetype = type, group = year),
    color = '#8C3D35',  size = 0.75, show.legend = FALSE
  ) + 
  geom_segment(
    aes(x = year, xend = year, y = -min(df_path$y), yend = prop + (min(df_path$y) * 3)),
    linetype = 3, color = '#8C3D35'
  ) +
  geom_text(
    aes(x = year - 0.1, y = (prop + (min(df_path$y) * 3))/2, 
        label = scales::percent_format(0.1)(prop)),
    angle = 90, family = f2, size = 4, color = '#BF6C5A'
  ) +
  ylim(c(-0.3, 3.5)) +
  
  # Title
  annotate(
    'text', 
    x = 2015.5, y = 3.15, label = 'CLUCK CLUCK CLUCK', 
    family = f1, size = 25, hjust = 0, color = '#8C3D35'
  ) +
  
  # Description
  ggtext::geom_textbox(
    data = data.frame(
      x = 2015.5,
      y = 2.65,
      label = paste0(
        'The production of eggs has two main purposes: consumption and incubation.<br>',
        "These two purposes are categorized as ",
        "**<span style='color:#8C3D35'>table eggs</span>** and ",
        "**<span style='color:#F29877'>hatching eggs</span>**."
      )
    ),
    aes(x = x, y = y, label = label),
    family = f2, size = 5, hjust = 0, fill = NA, box.color =  NA, 
    lineheight = 1.2, width = unit(1, 'npc')
  ) +
  
  ggtext::geom_textbox(
    data = data.frame(
      x = 2015.5,
      y = 2.1,
      label = paste0(
        'Until 2017, the production of eggs for hatching in the USA exceeded ',
        '<br>the production of table eggs. However, in 2018, there was a ',
        'reversal<br>and, since then, the production of table eggs has ',
        'been surpassing<br>the production of hatching eggs. By the end ',
        'of 2021, the production of<br>table eggs surpasses the hatching egg ',
        'production by a ratio of 300%!'
      )
    ),
    aes(x = x, y = y, label = label),
    family = f2, size = 5, hjust = 0, fill = NA, box.color =  NA, 
    lineheight = 1.2, width = unit(1, 'npc')
  ) +
  
  # Ratio text
  geom_text(
    data = data.frame(
      x = 2016, y = 1.15, label = 'ratio of table eggs\nto hatching eggs'
    ),
    aes(x = x, y = y, label = label), color = '#BF6C5A',
    lineheight = 1, family = f2, hjust = 0.5, size = 3.5, fontface = 'italic'
  ) +
  geom_curve(
    data = data.frame(x = 2015.6, xend = 2015.75, y = 1.1, yend = 0.25),
    aes(x = x, y = y, xend = xend, yend = yend), curvature = 0.5,
    color = '#BF6C5A', arrow = arrow(length = unit(0.015, 'npc'))
  ) +
  
  coord_fixed() +
  labs(
    caption = 'Source: THE HUMANE LEAGUE · Graphic: Matheus S. Rodrigues',
    y = NULL, x = NULL
  ) +
  theme(
    text = element_text(family = f2),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(rep(30, 4)),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = '#8C3D35'),
    plot.caption = element_text(
      size = 9, color = 'grey40', margin = margin(t = 20), hjust = 0.1
    ),
    axis.text.y = element_blank()
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2023/week_15/plot.png'),
  plot = p1,
  width = 12,
  height = 9,
  dpi = 300
)
