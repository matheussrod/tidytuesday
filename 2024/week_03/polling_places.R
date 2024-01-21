
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 3)
polling_places <- tuesdata$polling_places

# Analysis ------------------------------------------------------------------------------------
polling_count_year <- polling_places |> 
  dplyr::filter(jurisdiction_type == 'county') |>
  dplyr::mutate(election_year = lubridate::year(election_date)) |> 
  dplyr::group_by(state, county_name, election_year) |> 
  dplyr::count(name = 'qty') |> 
  dplyr::ungroup() |> 
  dplyr::mutate(state = state.name[match(state, state.abb)]) |> 
  dplyr::mutate(dplyr::across(state:county_name, stringr::str_to_sentence)) |> 
  dplyr::mutate(color = as.factor(dplyr::ntile(qty, 5)))

polling_count_color <- polling_count_year |> 
  dplyr::arrange(state, county_name, election_year) |> 
  dplyr::group_by(state, county_name) |> 
  dplyr::mutate(
    group_row = dplyr::row_number(),
    last_qty = ifelse(is.na(county_name), NA_integer_, qty - dplyr::lag(qty))
  ) |> 
  dplyr::filter(group_row > 1) |> 
  dplyr::mutate(color = dplyr::case_when(
    last_qty == 0 ~ '#D9C7B8',
    last_qty > 0 ~ '#04738C',
    last_qty < 0 ~ '#F20707'
  ))

map_usa <- maps::map('county', plot = FALSE, fill = TRUE)
map_usa_sf <- map_usa |> 
  sf::st_as_sf() |> 
  tidyr::separate_wider_delim(ID, names = c('state', 'county_name'), delim = ',') |> 
  dplyr::mutate(dplyr::across(state:county_name, stringr::str_to_sentence))

usa_county_years <- expand.grid(
  unique(map_usa_sf$state), 
  unique(map_usa_sf$county_name), 
  seq(2014, 2020, 2)
) |> dplyr::select(state = Var1, county_name = Var2, election_year = Var3)

map_usa_polling <- map_usa_sf |> 
  dplyr::left_join(usa_county_years, by = c('state', 'county_name')) |> 
  dplyr::left_join(polling_count_color, by = c('state', 'county_name', 'election_year')) |> 
  sf::st_as_sf()

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f_title <- 'outfit'
f_general <- 'noto'

showtext::showtext_opts(dpi = 100)
p1 <- ggplot() + 
  geom_sf(
    data = map_usa_polling,
    aes(fill = color), lwd = 0.025, show.legend = FALSE
  ) +
  scale_fill_identity(na.value = 'white') +
  facet_wrap(~election_year, ncol = 2, strip.position = 'top') +
  theme_void() +
  labs(
    title = 'How have polling location counts per county changed\nover time?',
    subtitle = paste0(
      "The polling locations in the United States were cataloged from 2012 to 2020. Over the ",
      "years, counties<br>experienced fluctuations in the number of polling locations. Below, the ",
      "evolution is depicted in comparison<br>to the previous election, categorized into three ",
      "groups: <b><span style='color:#F20707'>decrease</span></b>, <b><span style='color:#04738C'>increase",
      "</span></b> and <b><span style='color:#D9C7B8'>no variation</span></b>.<br><br>",
      "<i>Note: Some states do not have data in this dataset. Several states (Colorado, ",
      "Hawaii, Oregon, Washington and Utah)<br>vote primarily by mail and have little or ",
      "no data in this colletion, and others were not available for other reasons.</i>"
    ),
    caption = 'Source: Center for Public Integrity · Graphic: Matheus S. Rodrigues'
  ) +
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    text = element_text(family = f_general),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(
      size = 25, family = f_title, face = 'bold', lineheight = 0.75, margin = margin(b = 5)
    ),
    plot.subtitle = ggtext::element_textbox(size = 12, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0),
    strip.text.x = element_text(size = 10)
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_03/plot.png'),
  plot = p1,
  width = 10,
  height = 8,
  dpi = 300
)
