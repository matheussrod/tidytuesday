

# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 2)
nhl_rosters <- tuesdata$nhl_rosters

# Analysis ------------------------------------------------------------------------------------
years <- 1918:2024
seasons <- paste0(years - 1, years)
df_seasons <- data.frame('year' = years, 'season' = as.integer(seasons))

height_season <- nhl_rosters |> 
  dplyr::select(team_code, season, position_type, shoots_catches, height_in_centimeters) |> 
  dplyr::group_by(season, position_type) |> 
  dplyr::summarise(height = mean(height_in_centimeters, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(df_seasons, by = 'season')

last_height <- height_season |> 
  dplyr::arrange(dplyr::desc(year)) |> 
  dplyr::group_by(position_type) |> 
  dplyr::summarise(
    year = dplyr::first(year),
    height = dplyr::first(height)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)

sysfonts::font_add_google('Outfit', 'outfit')
f1 <- 'outfit'

showtext::showtext_opts(dpi = 100)
showtext::showtext_auto()
p1 <- ggplot(height_season) +
  geom_line(
    aes(x = year, y = height, color = position_type),
    linewidth = 1, show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(round(min(years), -1), round(max(years), -1), 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    sec.axis = dup_axis(
      breaks = last_height$height,
      labels = stringr::str_to_title(last_height$position_type),
      name = NULL
    )
  ) +
  colorblindr::scale_color_OkabeIto() +
  coord_cartesian(clip = 'off') +
  labs(
    title = 'Evolution of the height of Canadian hockey players',
    subtitle = 'Analysis segregated by position: goalkeepers, defenders and forwards',
    caption = 'Source: Statistics Canada · Graphic: Matheus S. Rodrigues',
    x = 'Season end year',
    y = 'Height (cm)'
  ) +
  theme(
    text = element_text(family = f1),
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(size = 25, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
    axis.text = element_text(size = 10),
    axis.text.y.right = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.line = element_line(size = 0.5, colour = 'grey30'),
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_02/plot.png'),
  plot = p1,
  width = 12,
  height = 8,
  dpi = 300
)
