
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 13)
public_picks <- janitor::clean_names(tuesdata$`public-picks`)

# Analysis ------------------------------------------------------------------------------------
picks_cumsum <- public_picks |> 
  dplyr::mutate(dplyr::across(r64:finals, ~as.double(stringr::str_remove(.x, '\\%')))) |> 
  tidyr::pivot_longer(r64:finals, values_to = 'perc') |> 
  dplyr::mutate(
    name = dplyr::case_match(
      name,
      'r64' ~ 1,
      'r32' ~ 2,
      's16' ~ 3,
      'e8' ~ 4,
      'f4' ~ 5,
      'finals' ~ 6
    )
  ) |> 
  dplyr::arrange(teamno, name) |> 
  dplyr::group_by(team, teamno) |> 
  dplyr::mutate(perc_cumsum = cumsum(perc)) |> 
  dplyr::ungroup()

picks_cumsum_7 <- picks_cumsum |> 
  dplyr::group_by(teamno) |> 
  dplyr::mutate(row_number = dplyr::row_number()) |> 
  dplyr::filter(row_number == 6) |> 
  dplyr::mutate(name = 7) |> 
  dplyr::select(-row_number)

picks_cumsum <- rbind(picks_cumsum, picks_cumsum_7)
team1 <- picks_cumsum[which.max(picks_cumsum$perc_cumsum),]$team
winner <- dplyr::filter(picks_cumsum, team == team1)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Graduate', 'graduate')
sysfonts::font_add_google('Lato', 'lato')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'graduate'
f2 <- 'lato'

p1 <- ggplot(picks_cumsum) +
  geom_step(
    aes(
      x = name,
      y = perc_cumsum,
      group = team,
    ),
    color = '#F26D3D',
    alpha = 0.2,
  ) +
  geom_step(
    data = winner,
    aes(
      x = name,
      y = perc_cumsum,
      group = team,
    ),
    color = '#F2F2F2',
    linewidth = 1,
  ) +
  geom_text(
    data = dplyr::filter(winner, name <= 6),
    aes(
      x = name + 0.5,
      y = perc_cumsum + 10,
      label = paste0(perc, '%'),
    ),
    size = 3,
    color = 'white',
    family = f2,
  ) +
  # title
  annotate(
    geom = 'text',
    x = 1,
    y = 420,
    label = 'The path of the favorite in\nMarch Madness',
    hjust = 0,
    vjust = 0,
    size = 7,
    color = 'white',
    family = f1,
    lineheight = 0.8,
  ) +
  # subtitle
  annotate(
    geom = 'text',
    x = 1, 
    y = 340,
    label = paste0(
      'According to the percentage of people who picked the\n',
      'team to win the games, ', team1 ,' is the favorite, with\n',
      winner$perc[6], '% of people choosing them in the final.\n\n',
      'Each line in graphic above represents a\nteam.'
    ),
    hjust = 0,
    vjust = 0,
    size = 3.75,
    color = 'white',
    family = f2,
    lineheight = 0.9,
  ) +
  # march madness description
  annotate(
    geom = 'text',
    x = 4.25,
    y = 30,
    label = paste0(
      'March Madness is the NCAA Division I basketball\n',
      'tournament for women and men. It\'s a single-\n',
      'elimination tournament of 68 teams that compete\n',
      'in six rounds for the national championship.'
    ),
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'grey80',
    family = f2,
    lineheight = 0.9,
    fontface = 'italic',
  ) +
  scale_x_continuous(
    breaks = seq(1.5, 6.5, 1),
    labels = c('r64', 'r32', 's16', 'e8', 'f4', 'finals'),
  ) +
  labs(
    x = NULL,
    y = NULL,
    caption = 'Source: Nishaan Amin · Graphic: Matheus S. Rodrigues',
  ) +
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(15, 4)),
    plot.background = element_rect(
         fill = '#73130A',
         color = NA,
    ),
    plot.caption = element_text(
         color = 'grey60',
         hjust = 0.085,
         margin = margin(t = 15),
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
         color = 'white',
    ),
    axis.text.y = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_13/plot.png'),
  plot = p1,
  width = 7,
  height = 8,
  dpi = 300,
)
