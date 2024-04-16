
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 16)
package_details <- janitor::clean_names(tuesdata$package_details)

# Functions -----------------------------------------------------------------------------------
get_version <- function(version, type = c('major', 'minor', 'patch')) {
  version_type <- match.arg(type)
  pattern <- '([0-9]+)\\.([0-9]+)\\.([0-9]+)'
  switch(version_type,
    major = stringr::str_extract(version, pattern, 1),
    minor = stringr::str_extract(version, pattern, 2),
    patch = stringr::str_extract(version, pattern, 3)
  )
}

# Analysis ------------------------------------------------------------------------------------
package_versions <- package_details |> 
  dplyr::mutate(version = stringr::str_replace(version, '-', '\\.')) |> 
  dplyr::mutate(version = stringr::str_replace(version, '\\d{4}\\.', '')) |> 
  dplyr::select(package, version) |> 
  dplyr::mutate(qty_dot = stringr::str_count(version, '\\.')) |> 
  dplyr::mutate(.version = dplyr::case_when(
    qty_dot == 1 ~ paste0(version, strrep(x = '.0', times = 1)),
    qty_dot == 0 ~ paste0(version, strrep(x = '.0', times = 2)),
    qty_dot > 2 ~ stringr::str_extract(version, '\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}'),
    .default = version
  )) |>
  dplyr::mutate(.version = ifelse(.version == '0.0.0', '0.0.1', .version)) |> 
  dplyr::mutate(
    major = get_version(.version, 'major'),
    minor = get_version(.version, 'minor'),
    patch = get_version(.version, 'patch')
  ) |> 
  dplyr::mutate(dplyr::across(major:patch, as.integer)) |> 
  dplyr::filter(dplyr::if_all(major:patch, ~ .x < 20))

versions_perc <- package_versions |> 
  dplyr::mutate(
    major1 = ifelse(major > 0, 1, 0),
    minor1 = ifelse(major > 0 | minor > 0, 1, 0),
    patch1 = ifelse(major > 0 | minor > 0 | patch > 1, 1, 0)
  ) |> 
  tidyr::pivot_longer(cols = major1:patch1) |> 
  dplyr::group_by(name) |> 
  dplyr::reframe(total = dplyr::n(), sum = sum(value)) |> 
  dplyr::mutate(perc = sum / total) |> 
  dplyr::mutate(name = stringr::str_replace(name, '\\d', '')) |> 
  dplyr::mutate(name = stringr::str_to_title(name)) |> 
  dplyr::mutate(label = dplyr::case_match(name,
    'Major' ~ 'of the packages have a stable\nversion, ready for production.',
    'Minor' ~ 'of the packages have implemented\nsome functionality',
    'Patch' ~ 'of the packages have progressed\nfrom the initial version 0.0.1',
  )) |> 
  dplyr::mutate(
    y = c(3, 1.75, 0.5) - 0.3,
    y_perc = y + 0.15,
    y_label = y - 0.25
  ) |> 
  dplyr::mutate(
    name_description = dplyr::case_match(name,
      'Major' ~ 'make incompatible API changes',
      'Minor' ~ 'add functionality',
      'Patch' ~ 'make backward compatible bug fixes'
    ),
    name_description = stringr::str_wrap(name_description, 20)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Lobster', 'lobster')
sysfonts::font_add_google('Abril Fatface', 'abril')
sysfonts::font_add_google('Titillium Web', 'titillium')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'lobster'
f2 <- 'abril'
f3 <- 'titillium'

p1 <- ggplot(versions_perc) + 
  # version name
  geom_text(
    aes(
      x = 0.15,
      y = y,
      label = name
    ),
    family = f2,
    size = 5,
    angle = 90,
    hjust = 0,
    vjust = 1,
  ) +
  # version name description
  geom_text(
    aes(
      x = 0.225,
      y = y + 0.27,
      label = name_description
    ),
    family = f3,
    size = 2,
    angle = 90,
    hjust = 0.5,
    vjust = 1,
    lineheight = 0.85,
  ) +
  # version percent
  geom_text(
    aes(
      x = 0.35,
      y = y_perc,
      label = paste0(round(perc * 100), '%')
    ),
    family = f2,
    size = 14,
    hjust = 0,
    vjust = 0,
  ) +
  # labels
  geom_text(
    aes(
      x = 0.35,
      y = y_label,
      label = label,
    ),
    family = f3,
    fontface = 'bold',
    size = 3,
    hjust = 0,
    vjust = 0,
  ) +
  # title
  annotate(
    geom = 'text',
    x = 0.1,
    y = 4.15,
    label = 'Shiny Packages\nMaturity',
    family = f1,
    fontface = 'bold',
    size = 10,
    hjust = 0,
    vjust = 0,
    lineheight = 0.75,
  ) +
  # subtitle
  annotate(
    geom = 'text',
    x = 0.1,
    y = 3.825,
    label = 'Semantic versioning of packages dependent on Shiny',
    family = f3,
    size = 2.75,
    hjust = 0,
    vjust = 0,
  ) +
  # caption
  annotate(
    geom = 'text',
    x = 0.9,
    y = -0.85,
    label = 'Source: R language\nGraphic: Matheus S. Rodrigues',
    family = f3,
    size = 2.75,
    hjust = 1,
    vjust = 0,
  ) +
  annotate(
    geom = 'rect',
    xmin = 0.1,
    xmax = 0.9,
    ymin = 3.6,
    ymax = 3.65,
    fill = 'black',
    linejoin = 'round',
  ) +
  annotate(
    geom = 'rect',
    xmin = 0.1,
    xmax = 0.9,
    ymin = -0.325,
    ymax = -0.35,
    fill = 'black',
    linejoin = 'round',
  ) +
  scale_x_continuous(
    limits = c(0, 1),
  ) +
  scale_y_continuous(
    limits = c(-1, 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
  ) +
  theme(
    plot.margin = margin(rep(15, 4)),
    plot.background = element_rect(
         fill = 'white',
         color = NA,
    ),
    panel.background = element_rect(
          fill = '#f3ecd9',
          color = NA,
    ),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
  )
showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_16/plot.png'),
  plot = p1,
  width = 4,
  height = 6,
  dpi = 300,
)
