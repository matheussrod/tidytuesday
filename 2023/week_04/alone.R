
# Alone -------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 04)
survivalists <- tuesdata$survivalists
loadouts <- tuesdata$loadouts

winners <- survivalists |> 
  dplyr::filter(result == 1) |> 
  dplyr::select(season, name)

winners_season4 <- winners |> 
  dplyr::filter(season == 4) |> 
  dplyr::pull(name) |> 
  paste(collapse = ' and ')

winners_loadouts <- loadouts |> 
  dplyr::inner_join(winners, by = c('season', 'name') ) |> 
  dplyr::select(season, name, item) |>
  dplyr::mutate(name = ifelse(season == 4, winners_season4, name)) |> 
  dplyr::distinct() |> 
  dplyr::arrange(season, item)

winners_items <- sort(unique(winners_loadouts$item), decreasing = TRUE)
items_rank <- dplyr::tibble(
  rank = seq_along(winners_items), 
  item = winners_items
)

winners_loadouts_rank <- dplyr::left_join(winners_loadouts, items_rank, by = 'item')

# Plot --------------------------------------------------------------------
library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Ubuntu', 'ubuntu')

f1 <- 'ubuntu'

winners_name <- unique(winners_loadouts$name)
facet_label <- as_labeller(setNames(stringr::str_wrap(winners_name, 15), seq_along(winners_name)))

p1 <- ggplot(winners_loadouts_rank) +
  geom_text(aes(x = 0, y = rank, label = item), family = f1, color = '#fefefe', size = 3.5) +
  facet_wrap(~ season, nrow = 1, labeller = facet_label) +
  labs(
    title = 'Winners\' loadout',
    subtitle = paste0(
      'On the Alone survival TV series each survivalist is allowed to take with them 10 items.\n',
      'Below is shown what the winners of each season took with them'
    ),
    caption = 'Source: Alone data package · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) + 
  theme(
    panel.border = element_rect(fill = NA, color = '#64675f'),
    panel.background = element_blank(), 
    panel.grid = element_blank(),
    plot.background = element_rect(fill = '#031114'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = f1, face = 'bold', color = '#fefefe', size = 25, hjust = 0.5),
    plot.subtitle = element_text(family = f1, size = 10, color = '#f2f0f1', hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(family = f1, size = 8, color = '#fefefe', hjust = 0.5),
    strip.background = element_rect(fill = NA, color = '#fefefe'),
    strip.text = element_text(family = f1, color = '#fefefe', size = 10)
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_04/plot.png"), 
  plot = p1, 
  width = 12, 
  height = 7,
  dpi = 300
)
