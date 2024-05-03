
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 18)
wwbi_data <- janitor::clean_names(tuesdata$wwbi_data)
wwbi_series <- janitor::clean_names(tuesdata$wwbi_series)
wwbi_country <- janitor::clean_names(tuesdata$wwbi_country)

# Analysis ------------------------------------------------------------------------------------

bra_world <- wwbi_data |>
  dplyr::filter(indicator_code == 'BI.PWK.AGES.PV.SM') |> 
  dplyr::mutate(country_code = dplyr::case_match(country_code,
    'BRA' ~ 'Brazil',
    .default = 'World'
  )) |> 
  dplyr::group_by(country_code, year) |> 
  dplyr::summarise(value = median(value)) |> 
  dplyr::ungroup()

range_year <- bra_world |> 
  dplyr::group_by(country_code) |> 
  dplyr::summarise(
    year_max = max(year), 
    year_min = min(year)
  )

bra_world_year <- bra_world |> 
  dplyr::filter(
    year == min(range_year$year_max) |
    year == max(range_year$year_min)
  )

perc_labels <- bra_world_year |> 
  dplyr::group_by(country_code) |> 
  dplyr::summarise(
    perc = (max(value) - min(value)) / min(value) * 100,
    x_mean = (max(value) + min(value))/2
  )

bra_world_plot <- bra_world_year |> 
  dplyr::inner_join(perc_labels, by = 'country_code') |> 
  dplyr::mutate(
    x = value, 
    y = rep(c(0, 5), each = 2),
    x_label = x_mean,
    label = paste0('+', round(perc, 0), '%')
  )

offset <- 1
bg_offset <- 1.35
polygons <- bra_world_plot |> 
  dplyr::distinct(country_code, x, y) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    x_pol = list(x + c(0, offset/2, offset, offset/2, 0) - offset / 2),
    y_pol = list(y + c(0, -offset/2, 0, offset/2, 0)),
    x_pol_background = list(x + c(0, bg_offset/2, bg_offset, bg_offset/2, 0) - bg_offset / 2),
    y_pol_background = list(y + c(0, -bg_offset/2, 0, bg_offset/2, 0))
  ) |> 
  tidyr::unnest(c(x_pol, y_pol, x_pol_background, y_pol_background)) |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(
    group = paste0(country_code, '_', round(x, 0)),
    fill = rep(c('#D97762', '#732D3A'), 2, each = 5)
  )

n_rects <- 1000
gradient_colors <- c('#D97762', '#F2CDC4', '#732D3A')
rects <- bra_world_plot |> 
  dplyr::group_by(country_code) |> 
  dplyr::summarise(
    xmin = min(x),
    xmax = max(x),
    ymin = min(y) - offset/2,
    ymax = max(y) + offset/2
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(xs = list(seq(xmin, xmax, length.out = n_rects))) |> 
  tidyr::unnest(xs) |> 
  dplyr::mutate(
    nxmin = xs,
    nxmax = xs + offset/75,
    fill = rep(colorRampPalette(gradient_colors, interpolate = 'spline')(n_rects), 2)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Roboto Slab', 'slab')
sysfonts::font_add_google('Lato', 'lato')
sysfonts::font_add_google('Open Sans', 'open')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'slab'
f2 <- 'lato'
f3 <- 'open'

p1 <- ggplot(bra_world_plot) +
  # gradient
  geom_rect(
    data = rects,
    aes(
      xmin = nxmin,
      xmax = nxmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
    ),
  ) +
  # polygon white background
  geom_polygon(
    data = polygons,
    aes(
      x = x_pol_background,
      y = y_pol_background,
      group = group,
      fill = 'white',
    ),
  ) +
  # valid polygons
  geom_polygon(
    data = polygons,
    aes(
      x = x_pol,
      y = y_pol,
      group = group,
      fill = fill,
    ),
  ) +
  # country labels
  geom_text(
    data = dplyr::summarise(
      bra_world_plot,
      x = min(x),
      y = min(y),
      .by = country_code
    ),
    aes(
      x = x - 0.75,
      y = y,
      label = country_code,
    ),
    family = f3,
    color = 'black',
    size = 4,
    hjust = 1,
    vjust = 0.5,
  ) +
  # percent labels
  geom_text(
    aes(
      x = x_label,
      y = y,
      label = label,
    ),
    family = f3,
    color = 'black',
    size = 4,
    check_overlap = TRUE,
  ) +
  ylim(c(-2.5, 7.5)) +
  xlim(
    c(
      min(bra_world_plot$value) - 1.5,
      max(bra_world_plot$value) + 1.25
    )
  ) +
  scale_fill_identity() +
  coord_fixed() +
  labs(
    title = 'Are private workers getting older?',
    subtitle = paste0(
      "It's possible to observe that from <span style='color:#d97762'>**2001**</span> to ",
      "<span style='color:#732d3a'>**2017**</span> the average age of paid private<br>employees",
      ' experienced a jump of 21%. In comparison to Brazil, this evolution was<br>slower, ',
      'around 12%. With this evolution, the average ages revolve around 40<br>years old.<br><br>',
      '**Are we working to live or living to work?**'
    ),
    y = NULL,
    x = NULL,
    caption = 'Source: Worldwide Bureaucracy Indicators (WWBI) · Graphic: Matheus S. Rodrigues',
  ) +
  theme(
    plot.margin = margin(t = 40, r = 0, b = 30, l = -30),
    plot.title = element_text(
         family = f1,
         size = 24,
         face = 'bold',
    ),
    plot.subtitle = ggtext::element_markdown(
         family = f2,
         size = 11,
         lineheight = 1.3,
    ),
    plot.caption = element_text(
         family = f2,
         size = 9,
         color = 'grey40',
         hjust = 0,
         margin = margin(t = 25),
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
          color = '#e6eaec',
    ),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
         family = f3,
         size = 8,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_18/plot.png'),
  plot = p1,
  width = 8,
  height = 8,
  dpi = 300,
)
