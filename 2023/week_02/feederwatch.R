
# FeederWatch -------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 2)
feederwatch <- janitor::clean_names(tuesdata$PFW_2021_public)
site_date <- janitor::clean_names(tuesdata$PFW_count_site_data_public_2021)

feederwatch_usa <- feederwatch |> 
  dplyr::select(loc_id, proj_period_id, latitude, longitude, subnational1_code, how_many) |> 
  dplyr::filter(stringr::str_detect(subnational1_code, 'US'), longitude > -130)

feederwatch_grouped <- feederwatch_usa |> 
  dplyr::group_by(loc_id, proj_period_id, subnational1_code, latitude, longitude) |> 
  dplyr::summarise(how_many = sum(how_many)) |> 
  dplyr::ungroup()

site_date_selected_cols <- site_date |> 
  dplyr::select(loc_id, proj_period_id, squirrels, cats, dogs, humans)

feederwatch_animals <- feederwatch_grouped |> 
  dplyr::left_join(site_date_selected_cols, by = c('loc_id', 'proj_period_id')) |> 
  dplyr::mutate(dplyr::across(squirrels:humans, ~ ifelse(is.na(.x), 0, .x))) |> 
  tidyr::pivot_longer(cols = c(squirrels, cats, dogs, humans),
    names_to = 'animal', values_to = 'flag') |>
  dplyr::mutate(how_many = ifelse(flag == 1, how_many, 0)) |> 
  dplyr::filter(flag == 1)

# from https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv
us_states <- readRDS(here::here('2023/week_02/us_states.rds'))

feederwatch_top1 <- feederwatch_animals |>
  dplyr::group_by(subnational1_code, animal) |> 
  dplyr::summarise(how_many = sum(how_many)) |> 
  dplyr::mutate(how_many_min = min(how_many)) |> 
  dplyr::filter(how_many == how_many_min) |> 
  dplyr::slice_head(n = 1) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(abbreviation = stringr::str_sub(subnational1_code, 4, -1)) |> 
  dplyr::left_join(us_states, by = 'abbreviation')

# sf::read_sf('https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/united-states.geojson')
usa_sf <- readRDS(here::here('2023/week_02/usa_geojson.rds'))
feederwatch_top1_sf <- dplyr::left_join(usa_sf, feederwatch_top1, by = c('name' = 'states'))


# Plot --------------------------------------------------------------------
library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Nosifer', 'nosifer')
font_add_google('Kanit', 'kanit')

f1 <- 'nosifer'
f2 <- 'kanit'

p1 <- ggplot(feederwatch_top1_sf) +
  geom_sf(aes(fill = animal), colour = 'grey95', size = 0.25) +
  scale_fill_manual(
    values = c('cats' = '#A02C2C', 'dogs' = '#E9AFAF', 'squirrels' = '#D35F5F'),
    name = NULL, labels = c('Cats', 'Dogs', 'Squirrels'),
    guide = guide_legend(direction = 'horizontal')
  ) +
  annotate(geom = 'text', x = -140, y = 48, label = 'The battle', size = 7.5, family = f1, color = 'darkred') +
  annotate(geom = 'text', x = -140, y = 43, label = 'cats, dogs and squirrels\nvs.\nbirds', family = f1, size = 3, color = 'darkred') +
  annotate(geom = 'text', x = -151, y = 37, 
           label = 'Between cat, dog and squirrel, which interfere\nmore in the observation of birds that visit feeders\nin the winter of 2021 in USA?', 
           family = f2, size = 2.5, hjust = 0, color = 'grey30') +
  annotate(geom = 'text', x = -151, y = 32, 
           label = 'The color indicates which animal caused the fewest\nobservations of the birds and the interference of the animals\nwas evaluated by checking if there were at least three interactions\nwith the feeders per week.', 
           family = f2, size = 2.5, hjust = 0, color = 'grey30') +
  coord_sf(xlim = c(-150, -70), clip = 'off') +
  labs(x = '', y = '', caption = 'Source: FeederWatch · Graphic: Matheus S. Rodrigues') +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(family = f2, color = 'grey30', size = 7, hjust = 0.05, vjust = 8),
    legend.position = c(0.625, -0.05),
    legend.box = 'horizontal',
    legend.text = element_text(family = f2, size = 8)
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_02/plot.png"), 
  plot = p1, 
  width = 10, 
  height = 5,
  dpi = 300
)
