
library(dplyr)
library(ggplot2)
library(showtext)

# African Language Sentiment ----------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 9)
afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages

perc_afrisenti <- afrisenti |> 
  count(language_iso_code, label, name = 'qty') |> 
  group_by(language_iso_code) |> 
  mutate(perc = qty / sum(qty)) |> 
  ungroup() |> 
  left_join(languages, by = 'language_iso_code')

inscribed_hexagon <- function(x, y, r, angle = pi/3) {
  ix <- x + r * cos(pi/6 + 0:5 * pi/3)
  iy <- y + r * sin(pi/6 + 0:5 * pi/3)
  angle <- pi/3
  points_x <- x + cos(angle) * (ix - x) - sin(angle) * (iy - y)
  points_y <- y + sin(angle) * (ix - x) + cos(angle) * (iy - y)
  return(list(x = points_x, y = points_y))
}

bspline_points <- function(df) {
  mutate(df,
    p1 = list(inscribed_hexagon(1, 0, positive)),
    p2 = list(inscribed_hexagon(2, 0, neutral)),
    p3 = list(inscribed_hexagon(3, 0, negative)),
    p4 = list(c(x = ((1 + positive) + (2 - neutral))/2, y = y_inter)),
    p6 = list(c(x = ((1 + positive) + (2 - neutral))/2, y = -y_inter)),
    p7 = list(c(x = ((2 + neutral) + (3 - negative))/2, y = y_inter)),
    p8 = list(c(x = ((2 + neutral) + (3 - negative))/2, y = -y_inter)),
    p9 = list(c(x = 3 + negative, y = 0)),
    p10 = list(c(x = 1 - positive, y = 0))
  ) |> 
    mutate(
      bspline_points = list(data.frame(
        x = c(
          p1$x[idx1], p4[1], p2$x[idx1], p7[1], p3$x[idx1], p9[1],
          p3$x[idx2], p8[1], p2$x[idx2], p6[1], p1$x[idx2], p10[1]
        ),
        y = c(
          p1$y[idx1], p4[2], p2$y[idx1], p7[2], p3$y[idx1], p9[2],
          p3$y[idx2], p8[2], p2$y[idx2], p6[2], p1$y[idx2], p10[2]
        )
      ))
    ) |> 
    tibble() |> 
    select(language:negative, bspline_points)
}

idx1 <- c(2, 1, 6)
idx2 <- 5:3
y_inter <- min(perc_afrisenti$perc) / 2
afrisenti_bspline <-  perc_afrisenti |> 
  select(language, label, perc) |> 
  tidyr::pivot_wider(names_from = label, values_from = perc) |> 
  select(language, positive, neutral, negative) |> 
  rowwise() |> 
  bspline_points()

# Plot --------------------------------------------------------------------
showtext_auto()
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'

bspline_plot <- function(df, var, percent_color = '#f5d7a9') {
  p <- ggplot(df) +
    ggforce::geom_bspline_closed(aes(x = x, y = y), fill = '#361c15') +
    geom_text(aes(x = 2, y = 0.7, label = language), color = '#f5d7a9', family = f1, size = 5, lineheight = 0.75, check_overlap = TRUE) +
    geom_text(aes(x = 1, y = 0, label = paste0(as.character(round(positive * 100)), '%')), color = percent_color, hjust = 0.5, size = 4, family = f1) +
    geom_text(aes(x = 2, y = 0, label = paste0(as.character(round(neutral * 100)), '%')), color = percent_color, hjust = 0.5, size = 4, family = f1) +
    geom_text(aes(x = 3, y = 0, label = paste0(as.character(round(negative * 100)), '%')), color = percent_color, hjust = 0.5, size = 4, family = f1) +
    coord_fixed() +
    ylim(c(-1, 1)) +
    theme_void(base_family = f1)
  assign(var, p, envir = globalenv())
}

afrisenti_bspline |> 
  tidyr::unnest(bspline_points) |>
  mutate(language = stringr::str_wrap(language, 20)) |> 
  group_split(language) |> 
  purrr::walk2(.y = paste0('p', 1:14), .f = bspline_plot)

p_assembly <- eval(str2lang(paste(paste0('p', 1:14), collapse = ' / '))) +
  patchwork::plot_layout(ncol = 4) +
  patchwork::plot_annotation(
    title = 'African Language Sentiment',
    subtitle = paste0(
      'Sentiment analysis for 14 African languages based ',
      'on tweets.\nThe tweets were categorized as positive, neutral, or ',
      'negative.\nBased on this categorization, the percentage of each',
      'category\nwas observed for each language.'
      ),
    caption = 'Source: AfriSenti · Graphic: Matheus S. Rodrigues',
    theme = theme(
      panel.background = element_rect(fill = '#a82f24'),
      plot.background = element_rect(fill = '#a82f24', color = NA),
      plot.margin = margin(rep(30, 4)),
      plot.title = element_text(family = f1, hjust = 0, color = '#f5d7a9', size = 28, margin = margin(b = 15)),
      plot.subtitle = element_text(family = f1, hjust = 0, color = '#f5d7a9', size = 14, margin = margin(b = 30)),
      plot.caption = element_text(family = f1, hjust = 0, color = '#f5d7a9', size = 12)
    )
  )

data.frame(language = 'How to read', positive = 0.33, neutral = 0.34, negative = 0.33) |> 
  rowwise() |>
  bspline_points() |>
  tidyr::unnest(bspline_points) |>
  bspline_plot('p_legend')

p_legend_label <- p_legend +
  annotate(
    'text', x = 1:3, y = -.4, label = c('Positive', 'Neutral', 'Negative'),
    family = f1, size = 3.5, color = '#f5d7a9'
  )

p_final <- cowplot::ggdraw(p_assembly) + 
  cowplot::draw_plot(p_legend_label, .6, .68, .4, .4, scale = .5)

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_09/plot.png"),
  plot = p_final, 
  width = 10, 
  height = 10,
  dpi = 300
)
