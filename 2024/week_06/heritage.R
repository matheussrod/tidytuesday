
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)
heritage  <- tuesdata$heritage

# Analysis ------------------------------------------------------------------------------------

c_2004 <- '#ee5440'
c_2022 <- '#3274d8'
n_countries <- nrow(heritage)

heritage_longer <- heritage |> 
  janitor::clean_names() |> 
  tidyr::pivot_longer(c(x2004, x2022))

heritage_x <- heritage_longer |>
  dplyr::mutate(
    x = rep(c(1, n_countries), n_countries),
    fill = rep(c(c_2004, c_2022), n_countries),
    order = dplyr::case_match(country,
      'Norway' ~ 3,
      'Sweden' ~ 2,
      'Denmark' ~ 1
    )
  )

space_between_rects <- 10
heritage_space <- heritage_x |> 
  dplyr::group_by(country) |> 
  dplyr::reframe(value_max = max(value), order = max(order)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(order) |> 
  dplyr::mutate(
    y_cumsum = dplyr::lag(cumsum(value_max)),
    y_space = seq(0, space_between_rects * 2, space_between_rects),
    y_cumsum_valid = ifelse(is.na(y_cumsum), 0, y_cumsum) + y_space
  ) |> 
  dplyr::select(country, y_cumsum_valid)

width <- 0.6
heritage_rect <- heritage_x |> 
  dplyr::inner_join(heritage_space, by = 'country') |> 
  dplyr::mutate(
    y_min = y_cumsum_valid,
    y_max = y_min + value,
    x_min = x - width,
    x_max = x + width
  )

c_gradient_colors <- c('#91b3e8', '#a7b0d7', '#f4a299')
n_gradient <- 1000

heritage_bezier <- heritage_rect |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    x = list(c(1 + width, 1 + width, 3 - width, 3 - width)),
    y = list(c(min(y_min), min(y_max),  max(y_max), min(y_min)))
  ) |> 
  tidyr::unnest(c(x, y))

bezier_gradient <- layer_data(
  ggplot(heritage_bezier) +
      ggforce::geom_diagonal_wide(aes(x = x, y = y, group = country), n = n_gradient) +
      facet_wrap(~country, ncol = 1)
  ) |>
  dplyr::select(x, y, group) |> 
  dplyr::mutate(xend = dplyr::lead(x)) |> 
  dplyr::mutate(
    color = rep(colorRampPalette(c_gradient_colors, interpolate = 'spline')(n_gradient * 2), n_countries),
    country = dplyr::case_match(group, 1 ~ 'Denmark', 2 ~ 'Norway', 3 ~ 'Sweden')
  ) |> 
  dplyr::filter(y != 0) |> 
  dplyr::inner_join(heritage_space, by = 'country') |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(y_begin = y_cumsum_valid)


df_y_text <- heritage_rect |> 
  dplyr::group_by(country) |> 
  dplyr::reframe(
    value_min = min(value),
    value_max = max(value),
    y_max = max(y_max), 
    y_text = y_max + 3
  )

base_text <- "<span style='font-size:7pt;'>World Heritage Sites</span>"
df_texts <- data.frame(
    x = rep(1:n_countries, each = n_countries),
    country = c('Norway', 'Sweden', 'Denmark')
  ) |> 
  dplyr::inner_join(df_y_text, by = 'country') |> 
  dplyr::mutate(
    increase = ((value_max - value_min)/value_min),
    decimal_label = (increase * 100) %% 1 != 0,
    increase_label = ifelse(
      decimal_label,
      scales::percent_format(accuracy = 0.1)(increase),
      scales::percent_format()(increase)
    )
  ) |> 
  dplyr::mutate(
    label = dplyr::case_when(
      x == 1 ~ paste0("<b><span style='font-size:10pt;'>", value_min, '</span></b>', '<br>', base_text),
      x == 2 ~ paste0("<b><span style='font-size:10pt;'>", increase_label, '</span></b>', '<br>', "<span style='font-size:7pt;'>Increase</span>"),
      x == 3 ~ paste0("<b><span style='font-size:10pt;'>", value_max, '</span></b>', '<br>', base_text)
    )
  )

df_texts_country <- df_texts |> 
  dplyr::group_by(country) |> 
  dplyr::reframe(x = 0, y = max(y_text))

df_v_lines <- data.frame(
  xmin = c(1 - width, 1 + width, 3 - width, 3 + width),
  xmax = c(1 - width, 1 + width, 3 - width, 3 + width),
  ymin = 0, 
  ymax = 62
)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Roboto', 'roboto')
sysfonts::font_add_google('Red Hat Display', 'work')
sysfonts::font_add_google('Raleway', 'raleway')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'roboto'
f2 <- 'work'
f3 <- 'raleway'

c_v_line <- '#e7eced'
c_text_aux <- '#6d7983'
p1 <- ggplot() +
  geom_rect(
    data = df_v_lines, 
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = c_v_line, linewidth = 0.3
  ) +
  geom_rect(
    data = heritage_rect,
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = fill)
  ) +
  geom_segment(
    data = bezier_gradient,
    aes(x = x, xend = xend, y = y_begin, yend = y, color = color),  linewidth = 0.1,
    show.legend = FALSE
  ) +
  ggtext::geom_richtext(
    data = df_texts,
    aes(x = x, y = y_text, label = label), hjust = 0.5,
    label.color = NA, label.padding = unit(rep(0, 4), 'pt'), vjust = 0, family = f2, size = 4
  ) +
  geom_text(
    data = df_texts_country,
    aes(x = x, y = y + 2.5, label = country), 
    vjust = 0, color = c_text_aux, family = f1, size = 3.2, hjust = .5
  ) +
  annotate(
    'text', 
    x = 1:3, y = 63.5, label = c('2004', 'Change', '2022'), 
    hjust = 0.5, color = c_text_aux, family = f3, size = 3
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    text = element_text(family = f1),
    plot.margin = margin(t = 40, r = 90, b = 40, l = 80)
  )


showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_06/plot.png'),
  plot = p1,
  width = 6,
  height = 7,
  dpi = 300
)

