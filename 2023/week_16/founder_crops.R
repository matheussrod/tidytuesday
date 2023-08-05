

# Neolithic Founder Crops -------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 16)
founder_crops <- tuesdata$founder_crops

df_base_curve <- founder_crops |> 
  dplyr::filter(!is.na(edibility)) |> 
  dplyr::mutate(
    age_start = round(age_start, -3), 
    age_end = round(age_end, -3)
  ) |> 
  dplyr::distinct(category, age_start, age_end, family) |> 
  dplyr::group_by(category, age_start, age_end) |> 
  dplyr::reframe(family = list(family)) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(family = stringr::str_wrap(paste(family, collapse = ', '), 30)) |> 
  dplyr::arrange(dplyr::desc(age_start), dplyr::desc(age_end)) |> 
  dplyr::filter(age_start != age_end) |> 
  dplyr::group_by(category) |> 
  dplyr::mutate(
    group = dplyr::row_number(),
    x_base = group %% 2,
    x_curve = ifelse(x_base == 0, -2.5, 2.5)
  ) |> 
  dplyr::ungroup()
  
wave <- dplyr::tibble(
  y = seq(
    from = min(df_base_curve$age_start, df_base_curve$age_end) - 1000,
    to = max(df_base_curve$age_start, df_base_curve$age_end) + 1000,
    by = 1
  ),
  x = sin(y/800)
)

df_bezier <- df_base_curve |> 
  dplyr::left_join(wave, by = c('age_start' = 'y')) |> 
  dplyr::group_by(category) |> 
  dplyr::mutate(
    diff = age_start - age_end,
    alpha = ifelse(diff == max(diff), 1, 0.9)
  ) |>
  dplyr::rowwise() |> 
  dplyr::mutate(
    x_bezier = ifelse(
      x_curve < 0,
      list(c(x, -2.8 - abs(x), -2.5 - abs(x) )),
      list(c(x, 2.8 + abs(x), 2.5 + abs(x)))
    ),
    y_bezier = list(c(age_start, (age_start + age_end)/1.9, age_end))
  ) |> 
  tidyr::unnest(c(x_bezier, y_bezier)) |> 
  dplyr::ungroup()

family_text <- df_bezier |> 
  dplyr::filter(alpha == 1) |> 
  dplyr::group_by(category, family) |> 
  dplyr::reframe(x_bezier = max(x_bezier), age_start = max(age_start)) |> 
  dplyr::mutate(
    angle = ifelse(x_bezier > 0, -80, 80),
    hjust = ifelse(x_bezier > 0, 0, 1),
    x_bezier = ifelse(x_bezier > 0, x_bezier, x_bezier - 2.5)
  )

library(ggplot2)
library(ggtext)
library(showtext)

showtext_auto()
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'


showtext_opts(dpi = 100)
p1 <- ggplot() +
  geom_path(data = wave, aes(x = x, y = y), linewidth = 1, color = 'grey85') +
  ggforce::geom_bezier(
    data = dplyr::filter(df_bezier, x_curve > 0),
    aes(
      x = x_bezier, y = y_bezier, group = group, linetype = 'cubic',
      color = age_start, alpha = alpha
    ),
    linewidth = 1, lineend = 'round', show.legend = FALSE
  ) +
  ggforce::geom_bezier(
    data = dplyr::filter(df_bezier, x_curve < 0),
    aes(
      x = x_bezier, y = y_bezier, group = group,
      color = age_start, alpha = alpha
    ),
    linewidth = 1, lineend = 'round', show.legend = FALSE
  ) +
  geom_text(
    data = family_text,
    aes(x = x_bezier, y = age_start - 500, label = family, angle = angle, hjust = hjust),
    size = 4, family = f1
  ) +
  facet_wrap(~category, nrow = 1) +
  xlim(c(-10, 10)) +
  scale_color_gradient(low = '#528C49', high = '#8C613B') +
  scale_y_continuous(
    breaks = seq(2000, 14000, by = 2000), 
    labels = function(x) paste0(x/1000, 'K')
  ) +
  labs(
    title = 'Neolithic Founding (Edible) Crops in southwest Asia',
    subtitle = paste0(
      'Each strip represents a set of families that were observed in the ',
      'same period.\nBelow, the families that had a longer period for each ',
      'category of edible crops are highlighted.'
    ),
    caption = 'Source: Arranz-Otaegui & Roe · Graphic: Matheus S. Rodrigues',
    y = 'Years before 1950', x = NULL
  ) +
  theme(
    text = element_text(family = f1),
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, color = 'grey35'),
    axis.title.y = element_text(size = 9, color = 'grey35', margin = margin(r = 10)),
    axis.line.y = element_line(color = 'grey80', linetype = 1),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.spacing.x = unit(-0.1, 'npc'),
    strip.text = element_text(size = 10),
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    plot.caption = element_text(
      size = 8, hjust = 0, color = 'grey50', margin = margin(t = 15)
    ),
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2023/week_16/plot.png'),
  plot = p1,
  width = 10,
  height = 8,
  dpi = 300
)
