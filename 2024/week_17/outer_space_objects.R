
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 17)
outer_space_objects <- janitor::clean_names(tuesdata$outer_space_objects)

# Functions -----------------------------------------------------------------------------------
expand_rect <- function(x, expand = 0.085) {
  dplyr::mutate(
    x, dplyr::across(
      c(xmin, xmax:ymax), 
      ~ ifelse(
        num_obj == max_per_continent, 
        .x + max_per_continent * expand,
        .x
      )
    )
  )
}

# Analysis ------------------------------------------------------------------------------------

continent_objects <- outer_space_objects |> 
  dplyr::filter(year >= 2020) |> 
  dplyr::mutate(continent = countrycode::countrycode(entity, 'country.name', 'continent')) |> 
  dplyr::arrange(continent, year) |> 
  dplyr::group_by(continent, year) |> 
  dplyr::reframe(num_obj = sum(num_objects)) |> 
  dplyr::mutate(
    min_max = ifelse(
      year == dplyr::first(year), 0,
      ifelse(year == dplyr::last(year), 1, NA_integer_)
    )
  ) |> 
  dplyr::filter(!is.na(min_max)) |> 
  tidyr::replace_na(list(continent = 'Others')) |> 
  dplyr::select(-min_max)

continents <- continent_objects |> 
  dplyr::mutate(continent = dplyr::case_when(
    continent %in% c('Europe', 'Africa', 'Oceania', 'Asia') ~ 'Africa, Oceania and Eurasia',
    .default = continent
  )) |> 
  dplyr::group_by(continent, year) |> 
  dplyr::summarise(num_obj = sum(num_obj)) |> 
  dplyr::ungroup()

xmin_continents <- continents |> 
  dplyr::group_by(continent) |> 
  dplyr::reframe(
    total = sum(num_obj),
    max_per_continent = max(num_obj)
  ) |> 
  dplyr::arrange(total) |> 
  dplyr::mutate(
    seq_x = 0:2,
    sep_x = 600
  ) |> 
  dplyr::mutate(
    lag = dplyr::lag(cumsum(max_per_continent)),
    lag = ifelse(is.na(lag), 0, lag),
    xmin = lag + seq_x * sep_x
  ) |> 
  dplyr::select(continent, xmin, max_per_continent)

continents_rect <- continents |> 
  dplyr::inner_join(xmin_continents, by = 'continent') |> 
  dplyr::mutate(
    xmax = xmin + num_obj,
    ymin = 0,
    ymax = num_obj
  ) |>
  expand_rect()

rect_shadow <- continents_rect |> 
  dplyr::group_by(continent) |> 
  dplyr::summarise(
    xmin = max(xmin),
    xmax = min(xmax) + max(max_per_continent) * 0.03,
    ymin = max(ymin),
    ymax = min(ymax) + max(max_per_continent) * 0.03
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(num_obj = NA_integer_)

continents_all_rects <- continents_rect |> 
  dplyr::select(continent, num_obj, xmin, xmax:ymax) |> 
  rbind(rect_shadow) |> 
  dplyr::arrange(dplyr::desc(xmin), dplyr::desc(xmax)) |> 
  dplyr::mutate(
    fill = c(
      '#283250', '#19233c', '#283250',
      #'#3274d8', '#2a69cf', '#3274d8',
      '#7d7461', '#635C51', '#7d7461',
      '#f05440', '#e24b3e', '#f05440'
    )
  ) |> 
  dplyr::group_by(continent) |> 
  dplyr::mutate(
    x_label = ifelse(xmin == min(xmin), min(xmax), max(xmax)),
    y_label = ifelse(ymax == min(ymax), min(ymax), max(ymax)),
    label = num_obj
  ) |> 
  dplyr::ungroup()

continents_labels <- continents_all_rects |> 
  dplyr::group_by(continent) |> 
  dplyr::summarise(
    x = (min(xmin) + max(xmax))/2,
    y = -200,
    label = stringr::str_wrap(dplyr::first(continent), 20)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
library(patchwork)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(continents_all_rects) + 
  # rects
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin, 
      ymax = ymax,
      fill = fill,
    ),
  ) +
  # values
  geom_text(
    aes(
      x = x_label,
      y = y_label,
      label = label,
    ),
    hjust = 1.2,
    vjust = 1.8,
    color = 'white',
    size = 3.5,
    family = f1,
  ) +
  # continents labels
  geom_text(
    data = continents_labels,
    aes(
      x = x,
      y = y,
      label = label,
    ),
    family = f1,
    size = 3.5,
    color = 'grey30',
    hjust = 0.5,
    vjust = 1,
    lineheight = 0.75,
  ) +
  scale_fill_identity() +
  ylim(c(-300, NA)) +
  coord_fixed() +
  labs(
    title = 'Objects Launched into Space: 2021 vs 2023',
    subtitle = paste0(
      'Objects are defined here as satellites, probes, landers, crewed spacecrafts, and space ',
      'station flight elements\nlaunched into Earth orbit or beyond.'
    ),
    caption = 'Source: United Nations Office for Outer Space Affairs · Graphic: Matheus S. Rodrigues',
  ) +
  theme_void() +
  theme(
    text = element_text(family = f1,),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(
         size = 25,
    ),
    plot.subtitle = element_text(
         size = 10,
    ),
    plot.caption = element_text(
         color = 'grey40',
         hjust = 0,
         margin = margin(t = 15),
    )
  )

# how to read
p2 <- ggplot() +
  # 2023
  annotate(
    'rect',
    xmin = 0.25,
    xmax = 1.25,
    ymin = 0.25,
    ymax = 1.25,
    fill = 'white',
    color = '#bcc7cd',
  ) +
  annotate(
    'text',
    x = 1.25,
    y = 1.25,
    label = '2023',
    hjust = 1.3,
    vjust = 1.8,
    size = 3,
  ) +
  # 2021
  annotate(
    'rect',
    xmin = 0,
    xmax = 0.8,
    ymin = 0,
    ymax = 0.8,
    fill = 'white',
    color = '#bcc7cd',
  ) +
  annotate(
    'text',
    x = 0.8,
    y = 0.8,
    label = '2021',
    hjust = 1.3,
    vjust = 1.8,
    size = 3,
  ) +
  annotate(
    'text',
    x = 0.625,
    y = -0.5,
    label = 'Continent or\nAgencies',
    hjust = 0.5,
    vjust = 0,
    size = 2.5,
    family = f1,
    lineheight = 0.8,
  ) +
  labs(title = 'How to read') +
  theme_void() +
  theme(
    text = element_text(
         family = f1,
    ),
    plot.title = element_text(
         size = 10,
         hjust = 0.5,
    ),
  )

p3 <- p1 + inset_element(
  p2, 
  0.025, 0.55, 0.125, 0.85
)

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_17/plot.png'),
  plot = p3,
  width = 8,
  height = 8,
  dpi = 300,
)
