
# Data ----------------------------------------------------------------------------------------
coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Functions -----------------------------------------------------------------------------------
build_smoke <- function(length, amplitude, frequency, offset) {
  y <- seq(0, length, length.out = 100)
  x <- amplitude * sin(frequency * pi * y) + offset
  smoke <- data.frame('smoke_x' = x, 'smoke_y' = y)
  return(smoke)
}

# Analysis ------------------------------------------------------------------------------------
## strength count -----------------------------------------------------------------------------
coffee_strength <- dplyr::select(coffee_survey, submission_id, strength)
strength_count <- coffee_strength |> 
  dplyr::count(strength) |> 
  dplyr::mutate(
    x = dplyr::case_match(
      strength,
      'Very strong' ~ 5,
      'Somewhat strong' ~ 4,
      'Medium' ~ 3,
      'Somewhat light' ~ 2,
      'Weak' ~ 1,
      .default = 0
    )
  ) |> 
  dplyr::arrange(x)

## cups ---------------------------------------------------------------------------------------
base_cup <- data.frame(
  x = c(0, 0.75, 2.25, 3),
  y = c(2, 0, 0, 2)
)

cups <- data.frame(cup_id = 0:5) |> 
  dplyr::cross_join(base_cup) |> 
  dplyr::mutate(
    x = x + cup_id * 4,
    y = y
  )

cups_middle <- cups |> 
  dplyr::group_by(cup_id) |> 
  dplyr::reframe(
    x = mean(x),
    y = mean(y)
  ) |> 
  dplyr::left_join(strength_count, by = c('cup_id' = 'x')) |> 
  dplyr::mutate(strength = ifelse(is.na(strength), 'Unknown', strength))

## smokes -------------------------------------------------------------------------------------
rescale <- \(x) scales::rescale(x, c(2.5, 20), c(0, max(cups_middle$n)))
df_smokes <- cups_middle |> 
  dplyr::mutate(max_n = max(n)) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    smoke = list(
      build_smoke(length = max_n, amplitude = 0.15, frequency = 1.1, offset = x)
    )
  ) |> 
  tidyr::unnest(smoke) |> 
  dplyr::mutate(
    dplyr::across(c(n, max_n, smoke_y), ~rescale(.x), .names = 'rescale_{.col}')
  ) |>
  dplyr::mutate(
    rescale_smoke_y = ifelse(
      rescale_smoke_y >= rescale_n,
      NA_integer_,
      rescale_smoke_y
    )
  )

## smokes text --------------------------------------------------------------------------------
smokes_text <- df_smokes |> 
  dplyr::group_by(cup_id) |> 
  dplyr::reframe(
    x = max(x),
    y = max(rescale_smoke_y, na.rm = TRUE),
    n = max(n),
  ) |> 
  dplyr::mutate(
    perc = n / sum(n),
    perc_label = scales::percent(perc),
    label = paste0(perc_label, '\n', '(', n, ')')
  )

## pallete ------------------------------------------------------------------------------------
pallete <- list(
  'background' = '#261206',
  'cup' = '#A6754B',
  'smoke' = '#F2E2CE',
  'text' = 'white'
)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Serif', 'noto')
sysfonts::font_add_google('Montserrat', 'montserrat')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'
f3 <- 'montserrat'

p1 <- ggplot() +
  ## geom: cups -------------------------------------------------------------------------------
  ggforce::geom_circle(
    data = cups_middle,
    aes(
      x0 = x + 1, 
      y0 = y,
      r = 0.5,
    ),
    fill = pallete$background,
    color = pallete$cup,
    size = 1.5,
  ) +
  geom_polygon(
    data = cups,
    aes(
      x = x,
      y = y,
      group = cup_id,
    ),
    fill = pallete$cup,
  ) +
  ## text: coffee strength --------------------------------------------------------------------
  geom_text(
    data = cups_middle,
    aes(
      x = x,
      y = -0.5,
      label = stringr::str_wrap(strength, 10),
    ),
    family = f3,
    size = 3,
    lineheight = 0.8,
    vjust = 1,
    color = pallete$text,
  ) +
  ## path: smokes -----------------------------------------------------------------------------
  ### 1º ----
  geom_path(
    data = df_smokes,
    aes(
      x = smoke_x,
      y = rescale_smoke_y,
      group = cup_id,
    ),
    color = pallete$smoke,
  ) +
  ### 2º ----
  geom_path(
    data = df_smokes,
    aes(
      x = smoke_x + 0.25,
      y = rescale_smoke_y,
      group = cup_id,
    ),
    color = pallete$smoke,
  ) +
  ### 3º ----
  geom_path(
    data = df_smokes,
    aes(
      x = smoke_x - 0.25,
      y = rescale_smoke_y,
      group = cup_id,
    ),
    color = pallete$smoke,
  ) +
  ## text: smokes -----------------------------------------------------------------------------
  geom_text(
    data = smokes_text,
    aes(
      x = x,
      y = y + 0.5,
      label = label,
    ),
    family = f1,
    color = pallete$text,
    hjust = 0.5,
    vjust = 0,
    size = 3,
    lineheight = 1,
  ) +
  ## title ------------------------------------------------------------------------------------
  annotate(
    'text',
    x = 0,
    y = 20,
    label = 'How strong do you\nlike your coffee?',
    family = f1,
    hjust = 0,
    vjust = 1,
    size = 8,
    color = pallete$text,
    lineheight = 0.9,
    fontface = 'bold',
  ) +
  ## subtitle ---------------------------------------------------------------------------------
  ggtext::geom_richtext(
    aes(
      x = 0,
      y = 17,
      label = paste0(
        'This was the question posed by James<br>',
        'Hoffmann and the coffee company<br>',
        'Cometeer in 2023 regarding 4 coffees<br>',
        'that requested for tasting. Based on<br>',
        'the responses, it is observed that<br>',
        '<b>55% of the respondents prefer<br>',
        'stronger coffees.</b>'
      ),
    ),
    family = f2,
    fill = 'transparent',
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    hjust = 0,
    vjust = 1,
    size = 3.5,
    color = pallete$text,
  ) +
  ## labs: caption ----------------------------------------------------------------------------
  labs(
    caption = 'Source: Cometeer · Graphic: Matheus S. Rodrigues',
  ) +
  ## coord ------------------------------------------------------------------------------------
  coord_fixed() +
  ylim(c(-2, 21)) +
  ## theme ------------------------------------------------------------------------------------
  theme_void() +
  theme(
    text = element_text(
         color = 'white',
    ),
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(
         fill = pallete$background,
         color = NA,
    ),
    plot.caption = element_text(
         family = f3,
         size = 8,
         color = 'grey70',
         hjust = 0,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_20/plot.png'),
  plot = p1,
  width = 7,
  height = 7,
  dpi = 300,
)
