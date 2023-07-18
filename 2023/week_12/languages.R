
# Programming Languages ---------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 12)
languages <- tuesdata$languages

library(ggplot2)
library(showtext)

find_top_n <- function(df, rank, n) {
  df |> 
    dplyr::arrange({{ rank }}) |> 
    dplyr::slice(seq(1, n))
}

top_n <- 12
offset <- 0.05

wiki_metrics <- languages |> 
  dplyr::filter(type == 'pl') |> 
  find_top_n(language_rank, top_n) |>
  dplyr::select(
    rank = 'language_rank', 
    'title', 
    'wikipedia_revision_count',
    'wikipedia_daily_page_views'
  ) |>
  tidyr::pivot_longer(-c(rank, title)) |> 
  dplyr::mutate(
    rank = rep(1:top_n, each = 2),
    name = ifelse(name == 'wikipedia_revision_count', 'revision count', 'daily page views'),
    x = rep(seq(1, top_n/3), top_n/4, each = 2),
    y = rep(seq(top_n/4, 1), each = 8) + rep(c(offset, -offset)),
    y_label = y + rep(c(offset, -offset)) * 1.5,
    start = rep(c(-pi/2, pi/2), top_n)
  )

title_rank <- wiki_metrics[c(seq(2, top_n*2, by = 2)), ] |> 
  dplyr::mutate(title_rank = paste0(title, ' (#', rank, ')'), y = y + offset) |> 
  dplyr::select(title_rank, x, y)


r <- 0.5
scale <- r/max(sqrt(wiki_metrics$value))

showtext_auto()
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'

p1 <- ggplot(wiki_metrics) + 
  ggforce::geom_arc_bar(
    aes(x0 = x, y0 = y, r0 = 0, r = sqrt(value) * scale,
        start = start, end = start + pi,
        fill = name
      )
  ) +
  scale_fill_manual(
    values = c('daily page views' = '#6A8FD9', 'revision count' = '#D97D5B')
  ) +
  geom_text(
    data = title_rank, 
    aes(x = x, y = y, label = title_rank),
    size = 11/.pt, vjust = 0.5, hjust = 0.5, family = f1
  ) +
  geom_text(
    aes(x = x, y = y_label, label = value), family = f1, color = '#fbfaf8'
  ) +
  coord_fixed() +
  guides(fill = guide_legend(title = 'Metric', reverse = TRUE)) +
  labs(
    title = 'Wikipedia metrics of the top 12 languages',
    subtitle = paste0(
      'Revision counts and daily pages views of the top 12 programming ',
      'languages on Wikipedia. Revision counts express\nhow many revisions ',
      'the page has and daily pages views express hits per day that Wikipedia ',
      'page has.'
    ),
    caption = 'Source: PLDB · Graphic: Matheus S. Rodrigues',
    x = '', y = '', fill = 'Metric'
  ) +
  theme(
    text = element_text(family = f1),
    plot.background = element_rect(fill = '#f1efe7', color = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'top',
    legend.text = element_text(size = 10),
    legend.background = element_blank()
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_12/plot.png"),
  plot = p1,
  width = 10,
  height = 10,
  dpi = 300
)

