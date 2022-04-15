

# Sources -------------------------------------------------------------------------------------------
 
# - https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-11
# - https://beeinformed.org/2021/06/21/united-states-honey-bee-colony-losses-2020-2021-preliminary-results/
# - https://ocm.auburn.edu/newsroom/news_articles/2021/06/241121-honey-bee-annual-loss-survey-results.php
# - https://www.theguardian.com/environment/2019/jun/19/us-beekeepers-lost-40-of-honeybee-colonies-over-past-year-survey-finds


# Main questions ------------------------------------------------------------------------------------

# - Are loss rates in the US similar to rates in other states?
# - Are stressors in other states the same as in the United States?


# Bee colonies data ---------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 2)
colony <- tuesdata$colony
stressor <- tuesdata$stressor


## Getting information only for other states and the United States ----------------------------------

states <- c("Other States", "United States")

columns_to_remove <- dplyr::all_of(c("colony_n", "colony_max", "colony_lost", "colony_added", "colony_reno"))

colony_states <- dplyr::filter(colony, state %in% states) |> 
  dplyr::select(-columns_to_remove)

# Getting only the main stressor for each year, month and state
stressor_states <- dplyr::filter(stressor, state %in% states) |> 
  dplyr::filter(!is.na(stress_pct)) |>
  dplyr::arrange(year, months, state, dplyr::desc(stress_pct)) |> 
  dplyr::group_by(year, months, state) |> 
  dplyr::summarise(stress_pct = max(stress_pct),
                   stressor = dplyr::first(stressor)) |> 
  dplyr::ungroup()

colony_stress <- colony_states |> 
  dplyr::inner_join(stressor_states, by = c("year", "months", "state"))


# Inspirations --------------------------------------------------------------------------------------

# 1.  [Bee With Ballonhand Finished Print of My Original Watercolour](https://pin.it/3AsyAnA)
# 2.  [Pattern Design Honey Bees & Bumble Bees — Amy Holliday Illustration](https://pin.it/5jm1VDg)


## Sketch -------------------------------------------------------------------------------------------

# img/bee_colonies.png


# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)
library(cowplot)
library(patchwork)
library(ggradar)
library(showtext)
library(ggtext)

year_months_abrev <- expand.grid(
  year = unique(colony_states$year), 
  months = unique(colony_states$months), 
  state = unique(colony_states$state)) |> 
  dplyr::mutate(abrev = factor(x = rep(rep(x = c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec"), each = 7, rep = 7), 2),
                               levels = c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec"))) |> 
  dplyr::as_tibble()

colony_abrev <- colony_stress |> 
  dplyr::right_join(year_months_abrev, by = c("year", "months", "state")) |> 
  dplyr::filter(year > 2015) |> 
  dplyr::select(-months) |> 
  dplyr::rename(months = abrev) |> 
  dplyr::relocate(months, .after = year) |> 
  dplyr::mutate(dplyr::across(.cols = colony_lost_pct:stress_pct, .fns = ~ ifelse(is.na(.x), 0, .x)),
                year = factor(year, levels = 2016:2021))
  
colony_split <- colony_abrev |> 
  dplyr::arrange(months) |> 
  dplyr::group_split(months) |>
  setNames(nm = paste0("p", 1:4))

radar_transformation <- function(df) {
  df <- df |> 
    dplyr::relocate(state, .before = year) |> 
    dplyr::select(state, year, colony_lost_pct) |> 
    tidyr::pivot_wider(names_from = year, values_from = colony_lost_pct, names_sort = TRUE)  
  
  return(df)
}

sysfonts::font_add_google("Fredoka One", "Fredoka")
sysfonts::font_add_google("Space Mono", "Space Mono")

f1 <- "Fredoka"
f2 <- "Space Mono"

showtext_auto()

hex_radar <- function(df, grid.min, grid.mid, grid.max, values.radar, group.colors, plot.legend = FALSE) {
  hex <- ggradar(
    plot.data = df,
    base.size = 10,
    values.radar = values.radar, 
    centre.y = 0,
    gridline.label.offset = -7,
    axis.labels = rep("", 6),
    grid.line.width = 0, 
    gridline.min.linetype = 0,
    gridline.mid.linetype = 0,
    gridline.max.linetype = 0, 
    group.line.width = 0,
    group.point.size = 0,
    group.colours = group.colors,
    background.circle.transparency = 0,
    axis.line.colour = rgb(0, 0, 0, alpha = 0, maxColorValue = 255),
    grid.label.size = 10,
    plot.legend = plot.legend,
    grid.min = grid.min,
    grid.mid = grid.mid,
    grid.max = grid.max
  ) + theme(panel.background = element_blank(), plot.background = element_blank())
  
  return(hex)
}

gg_hex <- function(group.colors, values.radar1 = rep("", 3), values.radar2 = rep("", 3), plot.legend = FALSE) {
  
  hex_lines <- tibble::tibble(
    state = c("Ohter States", "United States"),
    `2016` = c(0, 10),
    `2017` = c(0, 10),
    `2018` = c(0, 10),
    `2019` = c(0, 10),
    `2020` = c(0, 10),
    `2021` = c(0, 10)
  )
  hex_lines2 <- dplyr::mutate(hex_lines, dplyr::across(-state, ~ .x + 20))
  
  hex_radar1 <- hex_radar(hex_lines, 0, 5, 10,  values.radar1, group.colors)
  hex_radar2 <- hex_radar(hex_lines2, 20, 25, 30, values.radar2, group.colors)
  hex_radar <- ggdraw() + 
    draw_plot(hex_radar2) +
    draw_plot(hex_radar1, scale = 1/3) + 
    theme(panel.background = element_blank(), plot.background = element_blank())
  
  return(hex_radar)
  
}

gg_radar <- function(df, transformation = TRUE, scale = 0.8, group.colors = "#eb8a3e", plot.legend = FALSE) {
  
  month <- unique(df$months)
  
  if (transformation) { df <-  radar_transformation(df) }
  
  radar <- ggradar(
    plot.data = df,
    values.radar = rep("", 3), 
    font.radar = f2,
    centre.y = 0,
    grid.line.width = 0, 
    gridline.min.linetype = 0,
    gridline.mid.linetype = 0,
    gridline.max.linetype = 0, 
    group.line.width = 0,
    group.point.size = 0,
    group.colours = c("#d86526", "#edb335"),
    background.circle.transparency = 0, 
    plot.legend = plot.legend,
    axis.label.size = 12.5,
    axis.label.offset = 1.15,
    fill = TRUE,
    axis.line.colour = rgb(245, 173, 0, alpha = 0, maxColorValue = 255),
    grid.min = 0,
    grid.mid = 15,
    grid.max = 30
  )
  
  radar <- ggdraw() +
    draw_plot(radar, scale = scale) +
    draw_plot(plot = gg_hex(group.colors, values.radar1 = c("", "", "10%"), values.radar2 = c("20%", "", "30%")), scale = scale) + 
    cowplot::draw_label(label = month, y = 0.9, size = 50, vjust = 0, fontfamily = f2)
  
  return(radar)
}

purrr::imap(.x = colony_split, .f = ~ assign(x = .y, value = gg_radar(.x), envir = globalenv()))
plot <- (p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 2) + 
  plot_annotation(
    title = 'Percent of total colonies lost',
    subtitle = "Comparison of the <span style='color:#d86526'>**USA**</span> with <span style='color:#edb335'>**other states**</span> from 2016 to 2021",
    caption = 'Source: USDA · Graphic: Matheus S. Rodrigues',
    theme = theme(
      plot.title = element_text(family = f1, size = 120, hjust = 0),
      plot.margin = margin(40, 40, 40, 40),
      plot.subtitle = element_markdown(size = 50, hjust = 0, margin = margin(t = 5, b = 30, l = 0)),
      plot.caption = element_text(size = 35, hjust = 0, margin = margin(5, 5, 5, 5), color = "grey40")
    )
  )

ggsave(filename = here::here("2022/week_02/plot.png"), plot = plot, width = 10, height = 10, units = "in", dpi = 320)
