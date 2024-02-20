
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 7)
gifts_gender  <- tuesdata$gifts_gender

# Analysis ------------------------------------------------------------------------------------
gender_longer <- gifts_gender |> 
  tidyr::pivot_longer(cols = -c(1:2)) |>
  janitor::clean_names() |> 
  dplyr::mutate(value = ifelse(gender == 'Men', -value, value)) |> 
  dplyr::mutate(name = dplyr::case_match(name,
    'GreetingCards' ~ 'Greeting cards',
    'EveningOut' ~ 'Evening out',
    'GiftCards' ~ 'Gift cards',
    .default = name
  ))

gender_order <- gender_longer |> 
  dplyr::group_by(name) |> 
  dplyr::summarise(value = sum(abs(value))) |> 
  dplyr::as_tibble() |> 
  dplyr::arrange(value) |> 
  dplyr::mutate(x = dplyr::row_number())

colors <- list(
  'men' = '#032859',
  'women' = '#733657'
)

width <- 0.25
y_offset <- 20
gender_axis <- gender_longer |> 
  dplyr::group_by(gender) |> 
  dplyr::inner_join(gender_order[, c(1, 3)], by = 'name') |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(
    xmin = x - width,
    xmax = x + width,
    ymin = ifelse(value < 0, -y_offset, y_offset),
    ymax = ifelse(value < 0, value - y_offset, value + y_offset),
    fill = ifelse(value < 0, colors$men, colors$women)
  )

df_text_rect <- gender_axis |> 
  dplyr::group_by(name) |> 
  dplyr::reframe(
    xmin = min(xmin),
    xmax = max(xmax),
    ymin = min(ymin),
    ymax = -min(ymin)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Roboto', 'roboto')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'roboto'
f2 <- 'noto'

max_y <- 60
values_breaks <- seq(-max_y - y_offset, max_y + y_offset, 20)
values_labels <- ifelse(
  abs(values_breaks) <= y_offset, '', paste0(as.character(abs(values_breaks) - y_offset), '%')
)

p1 <- ggplot(gender_axis) + 
  geom_hline(
    yintercept = values_breaks[which(values_breaks < -y_offset | values_breaks > y_offset)],
    color = 'grey70', linetype = 2, linewidth = 0.25
  ) +
  geom_rect(
    data = df_text_rect,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color),
    fill = 'transparent', color = 'grey70', linetype = 2, linewidth = 0.25
  ) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = fill),
    key_glyph = draw_key_point, show.legend = FALSE
  ) +
  geom_text(
    aes(x = rep(1:(nrow(gender_axis)/2), 2), y = 0, label = name),
    hjust = 0.5, vjust = 0.5, family = f2, size = 3, check_overlap = TRUE
  ) +
  ggtext::geom_richtext(
    data = data.frame(
      x = nrow(gender_axis)/2 + 0.65,
      y = c(-y_offset, y_offset),
      hjust = c(1, 0),
      label = c(
        paste0("**<span style='color:", colors$men, '\'>Men</style>**'), 
        paste0("**<span style='color:", colors$women, '\'>Women</style>**')
      )
    ),
    aes(x = x, y = y, label = label, hjust = hjust), 
    label.padding = unit(rep(0, 4), 'pt'), label.color = NA, family = f2
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = values_breaks,
    labels = values_labels,
    limits = c(-max_y - y_offset, max_y + y_offset),
    expand = c(0, 0)
  ) +
  scale_fill_identity(aesthetics = c('color', 'fill')) +
  labs(
    title = 'Valentine\'s Day: Men vs Women',
    subtitle = 'How each gender spends proportionately (time and/or money) on gifts',
    caption = 'Source: NRF · Graphic: Matheus S. Rodrigues'
  ) +
  theme(
    text = element_text(family = f2),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(
      family = f1, face = 'bold', size = 22, hjust = 0.5, margin = margin(b = 5)
    ),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 15)),
    plot.caption = element_text(margin = margin(t = 20), hjust = 0.5),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_07/plot.png'),
  plot = p1,
  width = 7,
  height = 7,
  dpi = 300
)
