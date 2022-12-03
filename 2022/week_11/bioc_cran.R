
# CRAN & Bioc data ----------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 11)
cran <- janitor::clean_names(tuesdata$cran)

tidyverse_packages <- c(
  'dplyr',
  'ggplot2',
  'forcats',
  'tibble',
  'readr',
  'stringr',
  'tidyr',
  'purrr'
)

#Sys.setlocale("LC_TIME", "English")
formats <- c('%Y-%m-%d %H:%M:%S UTC', '%a %b %e %H:%M:%S %Y')
cran_dt <- cran |> 
  dplyr::filter(package %in% tidyverse_packages) |>
  dplyr::mutate(date_adj = as.Date(date, formats[[2]]),
                date_adj = ifelse(is.na(date_adj), as.Date(date, formats[[1]]), date_adj),
                date_adj = as.Date(date_adj, origin = '1970-01-01')) |> 
  dplyr::select(package, date = date_adj, version)

cran_days_new_version <- cran_dt |> 
  dplyr::arrange(package, date, version) |> 
  dplyr::group_by(package) |> 
  dplyr::mutate(next_date = dplyr::lead(date),
                days = as.integer(dplyr::lead(date) - date),
                days = ifelse(is.na(days), 0, days),
                gradient = scales::rescale(days, c(0, 1))) |> 
  dplyr::ungroup()


# Plot --------------------------------------------------------------------

library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Roboto', 'roboto')

p1 <- ggplot(cran_days_new_version) +
  geom_rect(aes(xmin = date, xmax = next_date, ymin = 0, ymax = 10, 
                fill = gradient)) + 
  scale_fill_gradient2(low = '#2C3532', mid = '#0F6466', high = '#D2E8E3',
                       name = 'Days to update',
                       labels = c(0, 200, 400, 600, 800)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
  facet_wrap(~ package, ncol = 1, strip.position = 'left') +
  labs(
    title = 'How intensive is the updating of tidyverse packages?',
    subtitle = 'Measured how many days it took to release a new version',
    caption = 'Source: Robert Flight · Graphic: Matheus S. Rodrigues',
    x = 'Date when package was uploaded/updated', y = ''
  ) +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    strip.background = element_blank(),
    strip.text.y.left = element_text(family = 'roboto', color = 'grey40', 
                                     size = 7, angle = 0),
    plot.title = element_text(family = 'roboto', face = 'bold', size = 15,
                              color = '#2C3532'),
    plot.subtitle = element_text(family = 'roboto', color = 'grey40', size = 10,
                                 margin = margin(b = 15)),
    plot.caption = element_text(family = 'roboto', face = 'italic', size = 8, 
                                hjust = 0, color = 'grey40', 
                                margin = margin(t = 15)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'roboto', color = 'grey40', size = 7),
    axis.title.x = element_text(family = 'roboto', color = 'grey40', size = 8),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = 'grey70'),
    legend.title = element_text(family = 'roboto', size = 8, 
                                margin = margin(b = 5)),
    legend.text = element_text(family = 'roboto', size = 7)
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2022/week_11/plot.png"), 
  plot = p1, 
  width = 9, 
  height = 6,
  dpi = 300
)
