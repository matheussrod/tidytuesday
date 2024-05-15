
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 19)
rolling_stone <- janitor::clean_names(tuesdata$rolling_stone)

# Analysis ------------------------------------------------------------------------------------
## top 10 ----
top10_albums <- rolling_stone |> 
  dplyr::select(
    clean_name,
    album,
    rank_2003:rank_2020,
    release_year,
    genre,
    weeks_on_billboard:peak_billboard_position,
    spotify_popularity,
    artist_gender,
    album_id
  ) |> 
  dplyr::mutate(image = paste0(here::here('2024/week_19/imgs/'),  '/', album_id, '.jpg')) |> 
  dplyr::filter(rank_2020 <= 10)

## album plot ----
albums_plot <- top10_albums |> 
  dplyr::arrange(rank_2020) |> 
  dplyr::mutate(genre = stringr::str_replace_all(genre, '\\/', '\\, ')) |> 
  dplyr::mutate(genre = stringr::str_replace_all(genre, '-', ' ')) |> 
  dplyr::mutate(
    x = rep(seq(0, 12.8, 3.2), 2),
    y = rep(c(5, 0), each = 5),
    rank_label = dplyr::case_when(
      rank_2020 == '1' ~ '1st',
      rank_2020 == '2' ~ '2nd',
      rank_2020 == '3' ~ '3rd',
      .default = paste0(rank_2020, 'th')
    ),
    genre = ifelse(is.na(genre), 'Not defined', genre),
    y_sep = 0.375,
    x_labels = x - 2,
    y_labels = y - 2,
    y_year = y_labels,
    y_weeks = y_labels - y_sep,
    y_peak = y_labels - 2 * y_sep,
    y_genre = y_labels - 3 * y_sep
  )

## spotify bar ----
spotify_bar <- albums_plot |> 
  dplyr::transmute(
    y_sep,
    spotify_popularity,
    xmin_bar = x_labels,
    xmax_bar = x_labels + 2.25,
    ymin_bar = y_labels + y_sep/2,
    ymax_bar = y_labels + y_sep,
    x_label = xmin_bar,
    y_label = ymax_bar + 0.5,
    label = ifelse(is.na(spotify_popularity), 'Spotify popularity¹', 'Spotify popularity')
  ) |>
  dplyr::mutate(
    x_spotify_popularity = ifelse(
      is.na(spotify_popularity), xmin_bar,
      (spotify_popularity * (xmax_bar - xmin_bar)) / 100 + xmin_bar
    )
  ) |> 
  dplyr::as_tibble()

plot_limit_x <- c(-2, 13.6)
plot_limit_y <- c(min(albums_plot$y) - 4.8, max(albums_plot$y) + 2.2)

plot_palette <- c(
  '#ef1c24',
  '#d93232',
  '#bf3030',
  'grey40',
  'grey85',
  'white',
  'black'
)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Abril Fatface', 'abril')
sysfonts::font_add_google('Noto Serif', 'noto')
sysfonts::font_add_google('Anton', 'anton')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'abril'
f2 <- 'noto'
f3 <- 'anton'

p1 <- ggplot(albums_plot) +
  ## plot: title ----
  annotate(
    'text',
    x = min(albums_plot$x_labels),
    y = 6.5,
    label = 'THE BEST ALBUMS OF ALL TIME ACCORDING TO ROLLING STONE IN 2020',
    size = 9,
    hjust = 0,
    vjust = 0,
    family = f3,
    color = plot_palette[1],
    fontface = 'bold',
  ) +
  ## rect: line bellow title and above caption ----
  annotate(
    'rect',
    xmin = rep(plot_limit_x[1], 2),
    xmax = rep(plot_limit_x[2], 2),
    ymin = c(6.125, -4),
    ymax = c(6.2, -4.075),
    fill = plot_palette[5],
  ) +
  ## geom: images ----
  ggimage::geom_image(
    aes(
      x = x,
      y = y,
      image = image,
    ),
    size = 0.135,
  ) +
  ## text: rank ----
  geom_text(
    aes(
      x = x - 1.6,
      y = y + 0.25,
      label = rank_label,
    ),
    family = f1,
    size = 6,
    hjust = 0.5,
    vjust = 0,
    color = plot_palette[2],
  ) +
  ## rect: line between rank and album ----
  geom_rect(
    aes(
      xmin = x - 2,
      xmax = x - 1.2,
      ymin = y + 0.09,
      ymax = y + 0.11,
    ),
    fill = plot_palette[3],
  ) +
  ## text: album ----
  geom_text(
    aes(
      x = x - 1.6,
      y = y,
      label = stringr::str_wrap(album, 10),
    ),
    family = f2,
    size = 3,
    hjust = 0.5,
    vjust = 1,
    color = plot_palette[2],
  ) +
  ## rect: spotify bar ----
  geom_rect(
    data = spotify_bar,
    aes(
      xmin = xmin_bar,
      xmax = xmax_bar,
      ymin = ymin_bar,
      ymax = ymax_bar,
    ),
    fill = plot_palette[6],
    color = plot_palette[7],
  ) +
  geom_rect(
    data = spotify_bar,
    aes(
      xmin = xmin_bar,
      xmax = x_spotify_popularity,
      ymin = ymin_bar,
      ymax = ymax_bar,
    ),
    fill = plot_palette[7],
  ) +
  geom_text(
    data = spotify_bar,
    aes(
      x = xmin_bar,
      y = ymax_bar + y_sep/2,
      label = label,
    ),
    family = f2,
    size = 3,
    hjust = 0,
    vjust = 0,
  ) +
  ## text: release year ----
  geom_text(
    aes(
      x = x_labels,
      y = y_year,
      label = paste0('Release year: ', release_year),
    ),
    family = f2,
    size = 3,
    hjust = 0,
    vjust = 1,
  ) +
  ## text: weeks bilboard ----
  geom_text(
    aes(
      x = x_labels,
      y = y_weeks, 
      label = paste0('Weeks on billboard: ', weeks_on_billboard),
    ),
    family = f2,
    size = 3,
    hjust = 0,
    vjust = 1,
  ) +
  ## text: peak bilboard ----
  geom_text(
    aes(
      x = x_labels,
      y = y_peak,
      label = paste0('Peak billboard position: ', peak_billboard_position),
    ),
    family = f2,
    size = 3,
    hjust = 0,
    vjust = 1
  ) +
  ## text: genre ----
  geom_text(
    aes(
      x = x_labels,
      y = y_genre,
      label = stringr::str_wrap(paste0('Genre: ', genre), 25),
    ),
    family = f2,
    size = 3,
    hjust = 0,
    vjust = 1,
  ) +
  ## rects: division ----
  geom_rect(
    aes(
      xmin = x + 0.9,
      xmax = x + 0.92,
      ymax = y + 0.85,
      ymin = y - 3.75,
    ),
    fill = plot_palette[5],
  ) +
  ## plot: caption ----
  annotate(
    'text',
    x = min(albums_plot$x_labels),
    y = plot_limit_y[1],
    label = paste0(
      'Source: Rolling Stone · Graphic: Matheus S. Rodrigues\n',
      '¹: Not on Spotify'
    ),
    size = 3,
    hjust = 0,
    vjust = 0,
    family = f2,
    color = plot_palette[4],
  ) +
  xlim(plot_limit_x) +
  ylim(plot_limit_y) +
  coord_fixed() +
  ## theme ----
  theme_void() +
  theme(
    plot.background = element_rect(
         fill = plot_palette[6],
         color = NA,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_19/plot.png'),
  plot = p1,
  width = 11,
  height = 8,
  dpi = 300,
)
