
# Soccer ------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 14)
soccer <- janitor::clean_names(tuesdata$`soccer21-22`)

selected_team <- 'Man City'

selected_home <- soccer |> 
  dplyr::filter(home_team == selected_team) |> 
  dplyr::select(team = away_team, team_goals = ftag, selected_goals = fthg) |> 
  dplyr::mutate(category = 'home')

selected_away <- soccer |> 
  dplyr::filter(away_team == selected_team) |> 
  dplyr::select(team = home_team, team_goals = fthg, selected_goals = ftag) |> 
  dplyr::mutate(category = 'away')


# Plot --------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(showtext)
library(ggforce)

scale <- 1/max(soccer$fthg, soccer$ftag)
create_df <- function(df, signal) {
  df |> 
    dplyr::select(-category) |> 
    dplyr::arrange(team) |> 
    dplyr::mutate(x = dplyr::row_number()) |> 
    tidyr::pivot_longer(c(team_goals, selected_goals), values_to = 'goal') |> 
    dplyr::mutate(
      y = (x %% 2 + 1) * signal,
      start = rep(c(pi/2, -pi/2), 19),
      end = start + pi,
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(goal_seq = list(0:goal)) |> 
    tidyr::unnest(goal_seq) |> 
    dplyr::mutate(r = goal_seq * scale * 0.75) |> 
    dplyr::mutate(
      r0 = ifelse(r == 0, 0, r - ((r - dplyr::lag(r))/1.25)),
      .by = c(team, name)
    )
}

create_line <- function(df, signal) {
  max_min <- if (signal < 0) max else min
  minus_plus <- if (signal < 0) `+` else `-`
  filter_name <- if (signal < 0) 'selected_goals' else 'team_goals'
  df |> 
    dplyr::filter(name == filter_name) |> 
    dplyr::group_by(team) |> 
    dplyr::summarise(y = max_min(y), r = max(r)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(y_max_line = minus_plus(minus_plus(y, r), 0.1))
}

input_y_offset <- function(df, signal) {
  offset <- 0.015
  check <- ifelse(signal < 0, 'selected_goals', 'team_goals')
  df |> 
    dplyr::mutate(
      y_offset = ifelse(name == check, y - offset * signal, y + offset * signal)
    )
}

filter_invalid_null <- function(df) {
  df |> 
    dplyr::mutate(max_goal = max(goal_seq), .by = c(team, name)) |> 
    dplyr::filter(!(goal_seq == 0 & max_goal != 0))
}

df_plot_top <- selected_home |> 
  create_df(1) |> 
  input_y_offset(1) |> 
  filter_invalid_null()

df_plot_down <- selected_away |> 
  create_df(-1) |> 
  input_y_offset(-1) |> 
  filter_invalid_null()

line_top <- df_plot_top |> create_line(1)
line_down <- df_plot_down |> create_line(-1)

showtext_auto()
font_add_google('Noto Sans', 'noto')
f1 <- 'noto'

p1 <- ggplot() +
  #plot for home
  ggforce::geom_arc_bar(
    data = df_plot_top,
    aes(x0 = x, y0 = y_offset, r0 = r0, r = r, start = start, end = end, fill = name),
    color = NA, show.legend = FALSE
  ) +
  
  #plot for away
  ggforce::geom_arc_bar(
    data = df_plot_down,
    aes(x0 = x, y0 = y_offset, r0 = r0, r = r, start = start, end = end, fill = name),
    color = NA, show.legend = FALSE
  ) +
  geom_text(
    data = df_plot_top, 
    aes(x = x, y = 0, label = team), size = 2.25, hjust = 0.5, vjust = 0.5,
    family = f1, check_overlap = TRUE
  ) +
  geom_segment(
    data = line_top, 
    aes(x = 1:19, xend = 1:19, y = 0.25, yend = y_max_line)
    ) +
  geom_segment(
    data = line_down, 
    aes(x = 1:19, xend = 1:19, y = -0.25, yend = y_max_line)
    ) +
  annotate('segment', x = 0, xend = 20, y = 3.6, yend = 3.6) +
  annotate(
    'label', x = 19/2, y = 3.6, label = 'Home', fill = 'white', 
    family = f1, label.size = NA, label.padding = unit(1, 'lines')
  ) +
  annotate('segment', x = 0, xend = 20, y = -3.2, yend = -3.2) +
  annotate(
    'label', x = 19/2, y = -3.2, label = 'Away', fill = 'white', 
    family = f1, label.size = NA, label.padding = unit(1, 'lines')
  ) +
  scale_fill_manual(
    values = c('selected_goals' = '#002e60', 'team_goals' = '#a8c9e8')
  ) +
  coord_fixed(ratio = 1) +
  ylim(c(-4, 4)) +
  labs(
    title = 'Manchester City F.C.',
    subtitle = paste0(
      'Premier League 2021 - 2022\n',
      'League Position: 1'
    ),
    caption = 'Source: Premier League · Graphic: Matheus S. Rodrigues | Inpired by Visual Athlete',
    x = '', y = ''
  ) +
  theme(
    text = element_text(family = f1),
    panel.background = element_blank(),
    plot.margin = margin(rep(15, 4)),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      face = 'bold', hjust = 0.475, color = '#002e60', size = 30,
      margin = margin(t = 10, b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.475, size = 10, margin = margin(b = -10)
    ),
    plot.caption = element_text(
      face = 'italic', hjust = 0.475, margin = margin(t = 30)
    )
  )

p_legend <- ggplot() +
  geom_arc_bar(
    aes(x0 = 1, y0 = 1, r0 = 0.2, r = 0.4, start = -pi/2, end = pi/2),
    fill = '#002e60', color = NA
  ) + 
  geom_arc_bar(
    aes(x0 = 2, y0 = 1, r0 = 0.2, r = 0.4, start = -pi/2, end = pi/2),
    fill = '#a8c9e8', color = NA
    ) + 
  geom_text(
    aes(x = 1:2, y = 0.925, label = c('Goal for\nManchester', 'Goal Against\nManchester')),
    family = f1, size = 2.5, vjust = 1, lineheight = 0.75
  ) +
  ylim(c(0.5, 1.5)) + xlim(c(0.5, 2.5)) +
  coord_fixed() + 
  theme_void()

p2 <- p1 + inset_element(p_legend, 0.275, 0.075, 0.7, 0.2, align_to = 'full')

showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2023/week_14/plot.png'),
  plot = p2,
  width = 14,
  height = 8,
  dpi = 300
)
