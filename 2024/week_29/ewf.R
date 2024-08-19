
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 29)
ewf_appearances <- janitor::clean_names(tuesdata$ewf_appearances)
ewf_standings <- janitor::clean_names(tuesdata$ewf_standings)

# Analysis ------------------------------------------------------------------------------------
last_appearence <- ewf_appearances |> 
  dplyr::filter(
    season == '2023-2024',
    division == 'Women\'s Super League (WSL)'
  )

end_pos <- last_appearence |> 
  dplyr::inner_join(
    dplyr::select(ewf_standings, team_name, position, season, division), 
    by = c('team_name', 'season', 'division')
  ) |> 
  dplyr::group_by(team_name) |> 
  dplyr::reframe(position = dplyr::last(position)) |> 
  dplyr::arrange(position)

rect_offset <- 0.75
df_plot <- last_appearence |> 
  dplyr::arrange(team_name, date) |> 
  dplyr::select(team_name, result, date) |> 
  dplyr::inner_join(end_pos, by = 'team_name') |> 
  dplyr::group_by(team_name) |> 
  dplyr::mutate(idx = dplyr::row_number()) |>
  dplyr::ungroup() |> 
  dplyr::mutate(
    team_name = stringr::str_squish(stringr::str_replace(team_name, 'Women', '')),
    team_name = stringr::str_c(team_name, ' (', position, 'º)'),
    team_name = forcats::fct_reorder(team_name, position),
    xmin = idx,
    xmax = idx + rect_offset,
    ymin = 0,
    ymax = 0 + rect_offset,
    fill = dplyr::case_match(
      result,
      'Loss' ~ 'darkred',
      'Draw' ~ 'grey',
      'Win' ~ 'darkgreen'
    ),
    hjust = 0,
    vjust = 0.5
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(df_plot) +
  
  ## rect -------------------------------------------------------------------------------------
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
    ),
  ) +
  
  ## wrap -------------------------------------------------------------------------------------
  facet_wrap(
    ~team_name, 
    ncol = 1, 
    strip.position = 'left',
  ) +
  
  ## scales and coord -------------------------------------------------------------------------
  scale_fill_identity(
    labels = c('Win', 'Lose', 'Draw'),
    guide = guide_legend(
      title = 'Result',
    )
  ) +
  coord_fixed() +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'Women\'s Super League (WSL) results',
    subtitle = paste0(
      'Results of the team\'s matches during the 2023-2024 season. Each square ',
      'represents a match.'
    ),
    caption = 'Source: The English Women\'s Football (EWF) Database · Graphic: Matheus S. Rodrigues',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme_void() +
  theme(
    text = element_text(
         family = f2,
    ),
    plot.background = element_rect(
         fill = 'white',
         color = NA,
    ),
    plot.margin = margin(rep(15, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f1,
         size = 18,
         face = 'bold',
    ),
    plot.subtitle = element_text(
         size = 11,
         margin = margin(t = 5, b = 10),
    ),
    plot.caption = element_text(
         size = 7,
         color = 'grey50',
         hjust = 1,
         margin = margin(t = 20),
    ),
    strip.text.y.left = element_text(
          hjust = 1,
          vjust = 0.5,
          angle = 0,
    ),
    legend.position = 'top',
    legend.key.size = grid::unit(12.5, 'pt'),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_29/plot.png'),
  plot = p1,
  width = 8,
  height = 5,
  dpi = 300,
)
