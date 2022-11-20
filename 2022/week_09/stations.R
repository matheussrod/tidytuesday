
# Stations data -----------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 9)
stations <- janitor::clean_names(tuesdata$stations)

stations_date_to_year <- function(x) {
  dt <- as.Date(
    stringr::str_c(stringr::str_extract(x, '\\d{4}/'), '01/01'),
    format = '%Y/%m/%d'
  )
  return(dt)
}

stations_fuel_levels <- stations |> 
  dplyr::mutate(
    fuel_type_code = factor(
      fuel_type_code, 
      levels = c('BD', 'CNG', 'HY', 'LNG', 'E85', 'ELEC', 'LPG')
    )
  )

stations_perc <- stations_fuel_levels |> 
  dplyr::filter(!is.na(open_date)) |> 
  dplyr::mutate(open_date_year = stations_date_to_year(open_date)) |>
  dplyr::count(open_date_year, fuel_type_code, name = 'qty') |> 
  dplyr::group_by(fuel_type_code) |> 
  dplyr::mutate(qty_cumsum = cumsum(qty)) |> 
  dplyr::group_by(open_date_year) |> 
  dplyr::mutate(perc = qty_cumsum / sum(qty_cumsum)) |> 
  dplyr::ungroup() |> 
  dplyr::select(open_date_year, fuel_type_code, perc)


# Plot --------------------------------------------------------------------

library(ggplot2)
library(ggstream)
library(showtext)

showtext_auto()
font_add_google('Roboto', 'roboto')

c1 <- 'grey95'
colors <- c(rep(c1, 5), "#F2CF63", c1)
p1 <- ggplot(stations_perc, aes(x = open_date_year, y = perc, fill = fuel_type_code)) +
  geom_stream() +
  scale_fill_manual(values = colors) +
  labs(
    title = 'Alternative fuel stations: the power of electricity',
    subtitle = 'With the emergence in 1995, the electric type has been gaining strength, reaching 93% of presence
in alternative fuel stations in 2022',
    caption = 'Source: US DOT · Graphic: Matheus S. Rodrigues',
    x = '', y = 'Percentage at alternative fuel stations'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey80', linetype = 3),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = 'roboto', face = 'bold', color = "#F2CF63", 
                              size = 15),
    plot.subtitle = element_text(family = 'roboto', color = 'grey40', size = 10, 
                                 margin = margin(b = 25)),
    plot.caption = element_text(family = 'roboto', color = 'grey40', hjust = 0),
    axis.title = element_text(family = 'roboto'),
    axis.title.y = element_text(size = 10, color = 'grey40'),
    axis.text = element_text(family = 'roboto', size = 8, color = 'grey40'),
    axis.ticks = element_line(color = 'grey65'),
    legend.position = 'none'
  )

showtext_opts(dpi = 500)
ggsave(
  filename = here::here("2022/week_09/plot.png"), 
  plot = p1, 
  width = 8, 
  height = 6,
  dpi = 500
)
