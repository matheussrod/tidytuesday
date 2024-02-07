
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 5)
groundhogs  <- tuesdata$groundhogs
predictions  <- tuesdata$predictions

# Analysis ------------------------------------------------------------------------------------

shadow_perc <- predictions |> 
  dplyr::filter(!is.na(shadow)) |> 
  dplyr::inner_join(groundhogs, by = 'id') |> 
  dplyr::filter(is_groundhog) |> 
  dplyr::filter(year >= 1900) |> 
  dplyr::group_by(year, shadow) |> 
  dplyr::count(name = 'qty') |> 
  dplyr::ungroup() |> 
  dplyr::mutate(shadow = ifelse(shadow, 'shadow_true', 'shadow_false')) |> 
  tidyr::pivot_wider(names_from = shadow, values_from = qty) |> 
  tidyr::replace_na(list(shadow_true = 0, shadow_false = 0)) |> 
  dplyr::arrange(year) |> 
  dplyr::mutate(
    perc_shadow = shadow_true/(shadow_true + shadow_false),
    row_number = dplyr::row_number()
  )

shadow_perc_angle <- shadow_perc |> 
  dplyr::mutate(groups = cut(year, breaks = 2)) |> 
  dplyr::group_by(groups) |> 
  dplyr::mutate(
    min_row = min(row_number),
    angle = (2 * pi * (row_number - min_row))/dplyr::n()
  ) |> 
  dplyr::ungroup()


t <- seq(5, -1, length.out = 500) * pi
spiral <- data.frame(
  x    = sin(t) * 1:500, 
  y    = cos(t) * 1:500,
  text = 'Winter or not winter, that is the question'
)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
library(patchwork)
sysfonts::font_add_google('Outfit', 'outfit')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)

f1 <- 'outfit'
c_winter <- '#032147'
c_spring <- '#fdce66'

nrow_df <- nrow(shadow_perc_angle)
p1 <- ggplot(
    shadow_perc_angle,
    aes(
      x = angle, xend = angle - 0.02,
      y = row_number/nrow_df, yend = row_number/nrow_df + 0.2,
      color = perc_shadow
    ),
  ) + 
  geom_segment(linewidth = 10) +
  geom_text(
    data = dplyr::filter(shadow_perc_angle, year == 1969),
    aes(x = angle, y = row_number/nrow_df - 0.02, label = '1970'), color = 'black',
    hjust = 1, vjust = 0.5, size = 3, angle = 42,
  ) +
  geom_segment(
    data = dplyr::filter(shadow_perc_angle, year == 1970),
    aes(
      x = angle, xend = angle, 
      y = row_number/nrow_df - 0.2, yend = row_number/nrow_df + 0.2
    ),
    color = 'black', linewidth = 0.2, alpha = 0.5
  ) +
  scale_color_gradient(
    low = c_spring,
    high = c_winter,
    breaks = seq(0, 1, 0.25),
    labels = c('Spring', rep('', 3), 'Winter'),
    guide = guide_colorbar(
      title = 'Groundhog predictions',
      title.position = 'top',
      direction = 'horizontal',
      barheight = 1,
      barwidth = 8,
      ticks = FALSE,
      nbin = 8
    )
  ) +
  coord_polar() +
  theme_void() +
  theme(
    text = element_text(family = f1),
    legend.position = c(0.5, 0.05),
    legend.title = element_text(hjust = 0.5, vjust = 0, size = 8, angle = 0),
    legend.text = element_text(size = 7)
  )

p_title <- ggplot(spiral) +
  geomtextpath::geom_textpath(
    data = spiral,
    aes(x = y, y = x, label = text), text_only = TRUE,
    size = 6, hjust = 0.6, vjust = 2, family = f1
  ) +
  coord_equal(clip = 'off') +
  theme_void()

p2 <- p1 + 
  inset_element(p_title, left = -0.17, bottom = 0, right = 1.075, top = 0.925) +
  plot_annotation(
    title = 'Happy Groundhog Day!',
    subtitle = "According to Groundhog Day tradition, if the groundhog<br>sees its shadow and
      goes back into its burrow, that is a<br>prediction of six more weeks of 
      <b><span style='color:#032147;'>winter</span></b>.
      Otherwise <b><span style='color:#fdce66;'>spring</span></b><br>will come early.<br><br>
      From 1900 to 2023, groundhog predictions were analyzed.<br>A significant change 
      can be observed from the 1970s<br>onwards, where there is a shift in forecasts,
      with spring<br>predominating.",
    caption = 'Source: groundhog-day.com · Graphic: Matheus S. Rodrigues',
    theme = theme(
      text = element_text(family = f1),
      plot.background = element_rect(fill = 'white'),
      plot.title.position = 'plot',
      plot.margin = margin(rep(20, 4)),
      plot.title = element_text(color = c_winter, size = 20, face = 'bold'),
      plot.subtitle = ggtext::element_markdown(margin = margin(b = -10)),
      plot.caption = element_text(hjust = 0.5, margin = margin(t = 20))
    )
  )


showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_05/plot.png'),
  plot = p2,
  width = 6.5,
  height = 6.5,
  dpi = 300
)
