
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 8)
isc_grants  <- tuesdata$isc_grants

stop_words <- readr::read_csv('https://raw.githubusercontent.com/vikasing/news-stopwords/master/sw100k.csv') |> 
  dplyr::filter(type == 'G', term != 'r') |>
  dplyr::pull(term)

# Analysis ------------------------------------------------------------------------------------
isc_summary <- strsplit(stringr::str_to_lower(isc_grants$summary), ' ')

remove_quotes <- \(x) stringr::str_replace_all(x, "\\'", '')
remove_double_quotes <- \(x) stringr::str_replace_all(x, '[\\"“”]', '')
remove_general <- \(x) stringr::str_replace_all(x, "[\\+\\/\\`\\[\\]\\{\\}\\(\\)\\.\\,\\'\\$\\&(***)\\%]*", '')

words <- purrr::map(
    isc_summary,
    purrr::compose(remove_general, remove_quotes, remove_double_quotes)
  ) |> 
  unlist() |> 
  table() |> 
  dplyr::as_tibble(.name_repair = janitor::make_clean_names) |> 
  dplyr::mutate(x = dplyr::case_when(
    x == 'packages' ~ 'package',
    x == 'used' ~ 'use',
    x == 'r' ~ 'R',
    .default = x
  )) |> 
  dplyr::group_by(x) |> 
  dplyr::reframe(n = sum(n)) |> 
  dplyr::arrange(dplyr::desc(n)) |> 
  dplyr::filter(n > 2) |> 
  dplyr::filter(!(x %in% stop_words))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Roboto', 'roboto')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'roboto'
f2 <- 'noto'

r_color <- '#2369bd'
mask <- png::readPNG(here::here('2024/week_08/mask.png'))
p1 <- ggplot(words, aes(label = x, size = n)) +
  ggwordcloud::geom_text_wordcloud_area(mask = mask, rm_outside = TRUE, color = r_color) +
  scale_size_area(max_size = 42.25) +
  labs(
    title = 'R Consortium ISC Grants',
    subtitle = paste0(
      'Most used words in the summaries for projects approved in the R Consortium\n',
      'Infrastructure ISC Grant Program since 2016'
    ),
    caption = 'Source: R Consortium · Graphic: Matheus S. Rodrigues'
  ) +
  theme(
    plot.margin = margin(rep(30, 4)),
    panel.background = element_blank(),
    plot.title = element_text(family = f1, size = 20, face = 'bold', color = r_color, hjust = 0.5),
    plot.subtitle = element_text(family = f2, size = 10, hjust = 0.5, color = r_color),
    plot.caption = element_text(family = f2, size = 7, hjust = 0.5, color = 'grey50')
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_08/plot.png'),
  plot = p1,
  width = 7,
  height = 7,
  dpi = 300
)
