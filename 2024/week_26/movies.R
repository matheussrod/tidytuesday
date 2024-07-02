
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 26)
lgbtq_movies <- janitor::clean_names(tuesdata$lgbtq_movies)

# Analysis ------------------------------------------------------------------------------------
movies <- lgbtq_movies |> 
  dplyr::mutate(
    language = dplyr::case_match(original_language,
      'en' ~ 'English',
      'es' ~ 'Spanish',
      'pt' ~ 'Portuguese',
      'fr' ~ 'French',
      .default = 'Others'
    ),
    language = ordered(
      language,
      c('English',
        'French',
        'Spanish',
        'Portuguese',
        'Others')
    )
  ) |> 
  dplyr::filter(vote_count > 0) |> 
  dplyr::mutate(color = ifelse(adult, '#A62D43', '#97A6A4'))

movies_text <- movies |> 
  dplyr::group_by(language, adult) |> 
  dplyr::reframe(
    mean = mean(vote_average)
  ) |> 
  dplyr::mutate(
    adult = ifelse(adult, 'yes', 'no'),
    mean = round(mean, 1)
  ) |> 
  tidyr::pivot_wider(
    names_from = adult,
    values_from = mean
  ) |> 
  dplyr::mutate(
    x = language,
    y = 11.5,
    label = paste0(
      'Vote average\n',
      'Adult: ', ifelse(is.na(yes), '-', yes), '\n',
      'Non adult: ', no
    ),
    hjust = 0.5,
    vjust = 1
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
howtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(movies) +

  ## jitter -----------------------------------------------------------------------------------
  geom_jitter(
    aes(
      x = language,
      y = vote_average,
      color = color,
    ),
    width = 0.35,
    height = 0.075,
    alpha = 0.35,
    show.legend = FALSE,
  ) +
  
  ## text -------------------------------------------------------------------------------------
  geom_text(
    data = movies_text,
    aes(
      x = x,
      y = y,
      label = label,
      hjust = hjust,
      vjust = vjust,
    ),
    family = f2,
    size = 2.65,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_y_continuous(
    breaks = seq(0, 10, 1),
    labels = as.character(seq(0, 10, 1)),
  ) +
  scale_color_identity() +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = paste0(
      "LGBTQ Movies: <span style='color:#a62d43'>adult</span> x ",
      "<span style='color:#97a6a4'>non adult</span>"
    ),
    subtitle = paste0(
      'Adult films have better ratings than non-adult films, especially\n',
      'English-language films.'
    ),
    caption = 'Source: The Movie Database (TMDB) · Graphic: Matheus S. Rodrigues',
    x = 'Movie language',
    y = 'Vote average',
  ) +
  ## theme ------------------------------------------------------------------------------------
  #theme_void() +
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(20, 4)),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(
          color = 'grey85',
    ),
    plot.title = ggtext::element_markdown(
         family = f1,
         size = 16,
         face = 'bold',
    ),
    plot.caption = element_text(
         family = f2,
         color = 'grey40',
         size = 8,
         margin = margin(t = 10),
    ),
    axis.ticks = element_blank(),
    axis.title = element_text(
         size = 9,
    ),
    axis.title.x = element_text(
         margin = margin(t = 5),
    )
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_26/plot.png'),
  plot = p1,
  width = 6,
  height = 7,
  dpi = 300,
)
