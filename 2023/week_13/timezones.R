
# Time Zones --------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 13)

transitions <- tuesdata$transitions

zones <- c(
  'America/Fortaleza',
  'America/Sao_Paulo',
  'America/Manaus',
  'America/Cuiaba',
  'America/Noronha',
  'America/Rio_Branco'
)

br_transitions <- transitions |> 
  dplyr::select(zone:end, abbreviation) |> 
  dplyr::filter(zone %in% zones, abbreviation != 'LMT') |> 
  dplyr::mutate(
    begin = lubridate::as_date(begin),
    end = lubridate::as_date(end),
    end = lubridate::as_date(ifelse(is.na(end), lubridate::today(), end)),
    abbreviation = as.integer(abbreviation),
    abbreviation_y = max(abbreviation), .by = zone,
    abbreviation_ymin = 0,
    abbreviation_ymax = ifelse(abbreviation_y == abbreviation, 2, -2),
    fill = ifelse(abbreviation_ymax == 2, 'top', 'down'),
    zone = dplyr::case_when(
      zone == 'America/Manaus' ~ 'North',
      zone == 'America/Rio_Branco' ~ 'North (Acre)',
      zone == 'America/Fortaleza' ~ 'Northeast',
      zone == 'America/Noronha' ~ 'Northeast (Noronha)',
      zone == 'America/Cuiaba' ~ 'Central-West',
      zone == 'America/Sao_Paulo' ~ 'Southeast'
    )
  )

br_transitions_south <- br_transitions |> 
  dplyr::filter(zone == 'Southeast') |> 
  dplyr::mutate(zone = 'South')

br_transitions_all <- rbind(br_transitions, br_transitions_south) |>
  dplyr::mutate(
    zone = factor(
      zone, 
      c(
        'North',
        'North (Acre)',
        'Northeast',
        'Northeast (Noronha)',
        'Central-West',
        'Southeast',
        'South'
      )
    )
  )

line <- br_transitions_all |> 
  dplyr::group_by(zone) |> 
  dplyr::summarise(
    begin = min(begin) - lubridate::days(2000),
    end = max(end) + lubridate::days(0),
    abbreviation_min = min(abbreviation),
    abbreviation_max = max(abbreviation)
  )

text <- line |> 
  tidyr::pivot_longer(dplyr::starts_with('abbreviation')) |> 
  dplyr::mutate(
    y = ifelse(name == 'abbreviation_max', 2, -2),
    x = begin,
    label = paste(value, 'UTC')
  )


# Plot --------------------------------------------------------------------

library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'

p1 <- ggplot(br_transitions_all) + 
  geom_segment(
    data = line, 
    aes(x = begin, xend = end, y = 0, yend = 0),
    color = 'grey85'
  ) +
  geom_rect(
    aes(xmin = begin, xmax = end, 
        ymin = abbreviation_ymin, ymax = abbreviation_ymax,
        fill = fill), color = '#403D39', size = 0.25,
    show.legend = FALSE
  ) +
  geom_text(
    data = text,
    aes(x = x, y = y, label = label),
    hjust = 0, size = 3, family = f1, color = 'grey30'
  ) +
  ylim(c(-7.5, 7.5)) +
  facet_wrap(~zone, ncol = 1, strip.position = 'left') +
  scale_fill_manual(values = c('top' = '#637AA6', 'down' = '#F2C1AE')) +
  labs(
    title = 'Brazil time zones transitions',
    subtitle = paste0(
      'How each geographic region has had its time zone changed from 1915',
      ' to 2023.'
    ),
    caption = 'Source: IANA tz database · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) +
  theme(
    text = element_text(family = f1),
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text.y.left = element_text(size = 10, angle = 0, hjust = 1),
    strip.background = element_blank(),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, hjust = 0),
  )


showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_13/plot.png"),
  plot = p1,
  width = 13,
  height = 7,
  dpi = 300
)
