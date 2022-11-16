
# Sources -----------------------------------------------------------------
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md

# Freedom data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 8)
freedom <- tuesdata$freedom


countries <- c(
  'Argentina',
  'Bolivia (Plurinational State of)',
  'Brazil',
  'Chile',
  'Colombia',
  'Ecuador',
  'Guyana',
  'Paraguay',
  'Peru',
  'Suriname',
  'Uruguay',
  'Venezuela (Bolivarian Republic of)'
)

freedom_south_america <- dplyr::filter(freedom, country %in% countries)


# Plot --------------------------------------------------------------------

library(ggplot2)
library(showtext)


CL_progress <- freedom_south_america |> 
  dplyr::arrange(country, year) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    first = dplyr::first(year),
    last = dplyr::last(year),
    CL_first = dplyr::first(CL),
    CL_last = dplyr::last(CL)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(progress = ifelse(CL_first < CL_last, 'increase', 'decrease')) |>
  tidyr::pivot_longer(cols = c('first', 'last'), values_to = 'year') |> 
  dplyr::mutate(CL = ifelse(name == 'first', CL_first, CL_last),
                country = stringr::str_extract(country, '([^\\s]+)')) |> 
  dplyr::select(country, year, CL, progress)

CL_df_list <- list(
  'increase' = dplyr::filter(CL_progress, progress == 'increase'),
  'decrease' = dplyr::filter(CL_progress, progress == 'decrease'),
  'y1995' = dplyr::filter(CL_progress, year == 1995),
  'y1995_increase' = dplyr::filter(CL_progress, year == 1995,
                                   progress == 'increase'),
  'y2020_increase' = dplyr::filter(CL_progress, year == 2020, 
                                    progress == 'increase')
)


showtext_auto()
font_add_google('Roboto', 'roboto')

p1 <- ggplot(CL_progress) + 
  geom_vline(xintercept = 1995, color = 'grey80') + 
  geom_vline(xintercept = 2020, color = 'grey80') +
  geom_line(aes(x = year, y = CL, group = country), data = CL_df_list$decrease, 
            color = 'gray80') +
  geom_line(aes(x = year, y = CL, group = country), data = CL_df_list$increase, 
            color = '#8C030E', size = 1) +
  geom_point(aes(x = year, y = CL), data = CL_df_list$y1995_increase, 
             size = 2, color = '#8C030E') +
  geom_point(aes(x = year, y = CL), data = CL_df_list$y2020_increase, 
             size = 2, color = '#8C030E') +
  geom_text(aes(x = year, y = CL, label = country), 
            data = CL_df_list$y2020_increase,
            hjust = -0.20, family = 'roboto') +
  scale_y_continuous(breaks = 1:7, labels = 1:7, limits = c(1, 7)) +
  scale_x_continuous(limits = c(1995, 2024)) +
  labs(
    title = 'Civil Liberties in South America from 1995 to 2020',
    subtitle = 'Only two countries worsened their civil liberties during the period: Venezuela and Guyana.

Scale: 1 = best, 7 = worst',
    caption = 'Source: Freedom House · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(size = 10),
    plot.margin = margin(t = 30, r = 50, b = 30, l = 50),
    plot.title = element_text(family = 'roboto', size = 15),
    plot.subtitle = element_text(family = 'roboto', size = 10, color = 'grey40', 
                                 margin = margin(b = 20)),
    plot.caption = element_text(family = 'roboto', color = 'grey40', hjust = 0),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = 'roboto', margin = margin(r = -10)),
    axis.ticks = element_blank()
  )

showtext_opts(dpi = 500)
ggsave(
  filename = here::here("2022/week_08/plot.png"), 
  plot = p1, 
  width = 8, 
  height = 6,
  dpi = 500
)
