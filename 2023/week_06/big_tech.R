
# Big Tech Stock Prices ---------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 06)
stock_prices <- tuesdata$big_tech_stock_prices

volume_year <- stock_prices |>
  dplyr::mutate(year = as.Date(lubridate::round_date(date, 'year'))) |>
  dplyr::group_by(year) |>
  dplyr::summarise(volume_total = sum(volume)) |>
  dplyr::ungroup()

apple <- stock_prices |>
  dplyr::mutate(year = as.Date(lubridate::round_date(date, 'year'))) |>
  dplyr::filter(year <= '2022-12-31') |>
  dplyr::left_join(volume_year, by = 'year') |>
  dplyr::group_by(stock_symbol, year) |>
  dplyr::summarise(volume = sum(volume),
                   volume_total = dplyr::first(volume_total)) |>
  dplyr::ungroup() |>
  dplyr::mutate(perc = volume / volume_total) |>
  dplyr::filter(stock_symbol == 'AAPL')

min_date <- lubridate::round_date(min(apple$year) - lubridate::years(1), 'year')
apple_polar_coord <- apple |>
  dplyr::add_row(
    stock_symbol = 'AAPL', year = min_date,
    volume = NA, volume_total = NA, perc = 0,
    .before = 1
  )

# Plot --------------------------------------------------------------------
library(ggplot2)
library(ggtext)
library(cowplot)
library(showtext)

showtext_auto()
font_add_google('Source Sans Pro', 'sans_pro')
f1 <- 'sans_pro'

pal <- c('#FEF5DF', '#F299A9', '#5A0B07')
pal_grey <- c('grey50', 'grey75', 'grey95')

plot_labels <- c('0%', '20%', '40%', '60%')
plot_labels_values <- seq(0, 0.6, 0.2)

p1 <- ggplot(apple_polar_coord) +
  geom_col(aes(x = year, y = perc, fill = perc), color = NA,
           show.legend = FALSE) +
  geom_hline(yintercept = plot_labels_values, colour = pal_grey[[3]],
             linewidth = 0.5) +
  annotate(
    'text', x = min_date, y = plot_labels_values + 0.05,
    label = plot_labels, color = pal_grey[[2]],
    hjust = 0.5, family = f1, size = 3
  ) +
  geom_point(
    data = data.frame(x = min_date, y = plot_labels_values),
    aes(x = x, y = y), color = pal_grey[[2]]
  ) +
  geom_richtext(
    data = dplyr::filter(apple_polar_coord, year > min_date),
    aes(x = year, y = perc + 0.125, color = perc,
        label = paste0(
          "<span style='font-size:8pt; color:#999999'>",
            format(year, '%Y'),
          '</span>', '<br>',
          '<b>', "<span style='font-size:14pt'>",
            as.character(round(perc * 100)), '%',
          '</span', '</b>'
        )
      ),
    fill = NA, label.color = NA, show.legend = FALSE
  ) +
  scale_color_gradient2(low = pal[[1]], mid = pal[[2]], high = pal[[3]]) +
  scale_fill_gradient2(low = pal[[1]], mid = pal[[2]], high = pal[[3]]) +
  scale_x_date(labels = NULL, expand = c(0.01, 0.01)) +
  scale_y_continuous(
    limits = c(-0.2, 0.7),
    breaks = c(0, 0.2, 0.4, 0.6),
    labels = NULL,
    expand = c(0.01, 0.01)
  ) +
  coord_polar(start = -pi/14, direction = -1) +
  labs(
    title = 'The Apple fall',
    subtitle = paste0(
      'Apple\'s percentage share of shares traded per year considering 14',
      'companies:\n', 'Apple, Adobe, Amazon, Salesforce, Cisco, Alphabet,',
      'International Business Machines Corporation\n', 'Intel, Meta, Microsoft',
      'Netflix, NVIDIA, Orace, and Tesla'
    ),
    caption = 'Source: Yahoo Finance · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) +
  theme(
    text = element_text(family = f1),
    panel.background = element_blank(),
    plot.margin = margin(t = 30, b = 30, l = 20, r = 20),
    plot.title = element_text(size = 25, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, 
                                 margin = margin(b = -20)),
    plot.caption = element_text(size = 9, color = pal_grey[[1]], hjust = 0.5,
                                margin = margin(t = -50)),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 7)
  )

logo_apple <- grid::rasterGrob(
  png::readPNG(here::here('2023/week_06/logo_apple.png'))
)

p2 <- ggdraw(p1) + draw_grob(logo_apple, 0.4775, 0.41, 0.07, 0.07)

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_06/plot.png"),
  plot = p2, 
  width = 7, 
  height = 7,
  dpi = 300
)
