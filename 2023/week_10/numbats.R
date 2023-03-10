
library(ggplot2)
library(showtext)

# Numbats in Australia ----------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 10)
numbats <- tuesdata$numbats

numbats_stats <- numbats |> 
  dplyr::select(year:hour) |> 
  dplyr::filter(!is.na(year)) |> 
  purrr::map_df(.f = ~.x[which.max(table(.x))])

t_numbats_stats <- dplyr::tibble(
  stat = colnames(numbats_stats),
  value = t(numbats_stats)[,1]
)

# Plot --------------------------------------------------------------------
showtext_auto()
font_add_google('Noto Sans', 'noto')
f1 <- 'noto'

rects <- \(x, l = 0.5) cumsum(c(x, rep(l, 3)))

p1 <- ggplot(numbats_stats) + 
  annotate(
    'rect', xmin = -0.1, xmax = 2.1, ymin = -0.1, ymax = 3.6, fill = '#f2dcb9'
  ) + 
  ggchicklet::geom_rrect(
    aes(xmin = 0, xmax = 2, ymin = 0, ymax = 3.5), fill = 'white', color = NA,
    radius = unit(0.025, 'npc')
  ) +
  ylim(c(-0.1, 3.6)) +
  annotate('text', x = 1, y = 1.5, label = 'Numbat', family = f1, size = 5) +
  annotate(
    'text', x = 1, y = 1.38, label = 'Myrmecobius fasciatus', family = f1,
    fontface = 'italic', size = 3
  ) +
  annotate(
    'text', x = 0.1, y = 1.2,
    label = 'Stats for when Numbats were most observed',
    family = f1, size = 2, fontface = 'italic', hjust = 0, color = 'grey30'
  ) +
  annotate(
    'rect', xmin = rects(0.06), xmax = rects(0.45), ymin = 1, ymax = 1.03,
    fill = '#998b75'
  ) +
  annotate(
    'text', x = (rects(0.06) + rects(0.45))/2, y = 1.085, 
    label = c('Year', 'Month', 'Weekday', 'Hour'), hjust = 0.5, size = 3, 
    family = f1
  ) +
  geom_text(
    data = t_numbats_stats,
    aes(x = (rects(0.06) + rects(0.45))/2, y = 0.95, label = value),
    hjust = 0.5, size = 2.5, family = f1
  ) +
  annotate(
    'text', x = 0.06, y = 0.5, label = paste0(
      'The species was once widespread across southern Australia,\n',
      'but is now restricted to several small colonies in Western Australia.\n',
      'It is therefore considered an endangered species and protected by\n',
      'conservation programs. Numbats were recently re-introduced to\n',
      'fenced reserves in South Australia and New South Wales. The\n',
      'numbat is the faunal emblem of Western Australia.'
    ), size = 2.5, hjust = 0, family = f1
  ) +
  annotate(
    'text', x = 1, y = 0.1, 
    label = 'Source: Atlas of Living Australia · Graphic: Matheus S. Rodrigues',
    hjust = 0.5, family = f1, size = 2, color = 'grey30'
  ) +
  coord_fixed() +
  theme_void(base_family = f1) +
  patchwork::inset_element(
    p = ggplot() + ggimage::geom_image(
      aes(x = 3.5, y = 5, image = here::here('2023/week_10/numbats.png')),
      size = 0.9
    ) + coord_fixed() + theme_void(), 
    0.1, 0.4, 0.9, 1
  ) + 
  patchwork::plot_annotation(theme = theme_void())

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_10/plot.png"),
  plot = p1, 
  width = 4, 
  height = 7,
  dpi = 300
)
