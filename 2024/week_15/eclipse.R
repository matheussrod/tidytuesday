
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 15)
eclipse_partial_2023 <- janitor::clean_names(tuesdata$eclipse_partial_2023)
eclipse_partial_2024 <- janitor::clean_names(tuesdata$eclipse_partial_2024)

# Analysis ------------------------------------------------------------------------------------

get_duration <- function(df) {
  df$duration <- with(
    data = df,
    expr = as.integer(eclipse_5 - eclipse_1) / 3600
  )
  return(df)
}

duration_2023 <- eclipse_partial_2023 |>
  get_duration() |> 
  dplyr::select(state, name, duration)

duration_2024 <- eclipse_partial_2024 |>
  get_duration() |> 
  dplyr::select(state, name, duration)

map_usa <- maps::map('county', plot = FALSE, fill = TRUE)
map_usa_sf <- map_usa |> 
  sf::st_as_sf() |> 
  tidyr::separate_wider_delim(ID, names = c('state', 'county_name'), delim = ',') |> 
  dplyr::mutate(dplyr::across(state:county_name, stringr::str_to_sentence))

usa_state <- data.frame(
  name = state.name,
  abb = state.abb
)

map_usa_duration_23 <- map_usa_sf |> 
  dplyr::left_join(usa_state, by = c('state' = 'name')) |> 
  dplyr::left_join(duration_2023, by = c('abb' = 'state', 'county_name' = 'name')) |> 
  dplyr::mutate(year = 'October 14, 2023')

map_usa_duration_24 <- map_usa_sf |> 
  dplyr::left_join(usa_state, by = c('state' = 'name')) |> 
  dplyr::left_join(duration_2024, by = c('abb' = 'state', 'county_name' = 'name')) |> 
  dplyr::mutate(year = 'April 8, 2024')

map_usa_duration <- rbind(map_usa_duration_23, map_usa_duration_24) |> 
  sf::st_as_sf()

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'

limit_legends <- with(
  map_usa_duration,
  c(min(duration, na.rm = TRUE), max(duration, na.rm = TRUE))
)

p1 <- ggplot() +
  geom_sf(
    data = map_usa_duration,
    aes(
      fill = duration,
    ),
    color = 'grey30',
    lwd = 0.1,
  ) +
  facet_wrap(
    ~year,
    ncol = 1,
    strip.position = 'left',
  ) +
  scale_fill_gradient2(
    low = '#8C3503',
    mid = '#F28627',
    high = '#F2C84B',
    midpoint = mean(limit_legends),
    na.value = '#957a5e',
    guide = guide_colorbar(
      title = 'Annular solar eclipse duration\n(in hours)',
      title.theme = element_text(
            family = f2,
            color = 'white',
            size = 7,
            lineheight = grid::unit(0.75, 'pt'),
      ),
      title.position = 'top',
      title.vjust = 1,
      barwidth = grid::unit(120, 'pt'),
      barheight = grid::unit(10, 'pt'),
      direction = 'horizontal',
      frame.colour = 'grey30',
    ),
  ) +
  labs(
    title = '2023 & 2024 US Partial Solar Eclipses',
    subtitle = paste0(
      'An annular solar eclipse occurs when the Moon passes between the\n',
      'Sun and Earth while at its farthest point from Earth. Because the\n',
      'Moon is farther away from Earth, it does not completely block the\n',
      'Sun. This create[d] a “ring of fire” effect in the sky for those\n',
      'standing in the path of annularity.'
    ),
    caption = 'Source: NASA · Graphic: Matheus S. Rodrigues',
  ) +
  theme(
    plot.background = element_rect(
         fill = '#957A5E',
         color = NA,
    ),
    plot.margin = margin(rep(30, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f1,
         size = 17.5,
         color = 'white',
    ),
    plot.subtitle = element_text(
         family = f2,
         color = 'white',
         size = 9,
         margin = margin(b = 65),
    ),
    plot.caption = element_text(
         family = f2,
         size = 7,
         color = 'white',
         hjust = 0,
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(
          fill = '#957A5E',
          color = NA,
    ),
    strip.text = element_text(
          family = f2,
          color = 'white',
          size = 8,
          hjust = 0.5,
          vjust = 0.5,
    ),
    legend.position = c(0.15, 1.1),
    legend.background = element_blank(),
    legend.title = element_text(
           color = 'white',
    ),
    legend.text = element_text(
           family = f2,
           color = 'white',
           size = 7,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_15/plot.png'),
  plot = p1,
  width = 5,
  height = 7,
  dpi = 300,
)
