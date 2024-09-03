
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 31)
summer_movie_genres <- janitor::clean_names(tuesdata$summer_movie_genres)
summer_movies <- janitor::clean_names(tuesdata$summer_movies)

# Analysis ------------------------------------------------------------------------------------
df_plot <- summer_movies |> 
  dplyr::select(-genres) |> 
  dplyr::inner_join(summer_movie_genres, by = 'tconst') |> 
  dplyr::group_by(genres) |> 
  dplyr::reframe(
    votes = sum(num_votes),
    rating = mean(average_rating)
  ) |> 
  dplyr::filter(!is.na(genres))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(df_plot) +
  
  ## col --------------------------------------------------------------------------------------
  geom_col(
    aes(
      y = forcats::fct_reorder(genres, votes),
      x = rating,
      fill = votes,
    ),
    alpha = 0.5,
    color = 'grey30',
  ) +
  
  ## annotate ---------------------------------------------------------------------------------
  ### talkshow --------------------------------------------------------------------------------
  annotate(
    'text',
    x = 7.5,
    y = 5,
    size = 3,
    label = paste0(
      'Talk-Show is the best\n',
      'genre according IMDb\n',
      'ratings'
    ),
    lineheight = 0.9,
    hjust = 0,
    vjust = 0,
  ) +
  
  annotate(
    'curve',
    x = 9,
    y = 5,
    xend = 8,
    yend = 2,
    arrow = grid::arrow(length = unit(7, 'pt'), type = 'closed'),
    curvature = -0.5,
    linewidth = 0.5,
  ) +
  
  ### horror ----------------------------------------------------------------------------------
  annotate(
    'text',
    x = 7.5,
    y = 16,
    size = 3,
    label = paste0(
      'Horror is the worst\n',
      'genre according IMDb\n',
      'ratings. However, it is\n',
      'fifth if considering\n',
      'votes (popularity)'
    ),
    lineheight = 0.9,
    hjust = 0,
    vjust = 0,
  ) +
  
  annotate(
    'curve',
    x = 9,
    y = 19.5,
    xend = 5,
    yend = 21,
    arrow = grid::arrow(length = unit(7, 'pt'), type = 'closed'),
    curvature = 0.2,
    linewidth = 0.5,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_fill_gradient(
    low = '#F2CBBD',
    high = '#733B36',
    breaks = seq(250000, 1250000, 250000),
    labels = paste0(c('0.25', '0.50', '0.75', '1.00', '1.25'), 'M'),
    guide = guide_colorbar(
      title = 'Votes',
      title.vjust = 0.75,
      barwidth = grid::unit(150, 'pt'),
      barheight = grid::unit(15, 'pt'),
      
    ),
  ) +
  
  scale_x_continuous(
    breaks = 0:10,
    labels = c(0:10),
    limits = c(0, 10),
    expand = c(0, 0.1),
  ) +
  
  coord_cartesian(
    clip = 'off',
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = paste0(
      'Does popularity equate to quality when it comes\n',
      'to Summer Movies?*?'
    ),
    y = 'Movie genre',
    x = 'Rating',
    caption = paste0(
      'Source: Internet Movie Database · Graphic: Matheus S. Rodrigues\n',
      '*Summer movies: movies with "summer" in their title'
    ),
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(25, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f1,
         face = 'bold',
         size = 18,
         margin = margin(b = 60),
    ),
    plot.caption = element_text(
         size = 8,
         color = 'grey40',
         hjust = 1,
         margin = margin(t = 10),
    ),
    panel.background = element_blank(),
    legend.position = c(0.1, 1.08),
    legend.direction = 'horizontal',
    axis.ticks.y = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_31/plot.png'),
  plot = p1,
  width = 6,
  height = 8,
  dpi = 300,
)
