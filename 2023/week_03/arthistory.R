
# Art History -------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 03)
artists <- tuesdata$artists

space_gender <- artists |> 
  dplyr::select(artist_gender, space_ratio = space_ratio_per_page_total) |> 
  dplyr::filter(artist_gender != 'N/A') |> 
  dplyr::group_by(artist_gender) |> 
  dplyr::summarise(space_ratio = mean(space_ratio)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    sqrt_space_ratio = sqrt(space_ratio),
    xmin = 0.5 - sqrt_space_ratio/2,
    ymin = 0.4 - sqrt_space_ratio/2,
    xmax = 0.5 + sqrt_space_ratio/2,
    ymax = 0.4 + sqrt_space_ratio/2
  )

# Plot --------------------------------------------------------------------
library(ggplot2)
library(showtext)
library(ggpattern)

showtext_auto()
font_add_google('Metamorphous', 'meta')
font_add_google('Redressed', 'redressed')

f1 <- 'meta'
f2 <- 'redressed'

p1 <- ggplot() +
  geom_rect_pattern(
    aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
    pattern_filename = here::here('2023/week_03/old_paper.jpg'),
    pattern = 'image',
    pattern_type = 'expand'
  )

p2 <- p1 + 
  geom_rect(
    data = space_gender,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, color = artist_gender),
    linewidth = 1.5, alpha = 0, show.legend = FALSE
  ) +
  scale_color_manual(values = c('Female' = '#2d190c', 'Male' = '#2d190c')) +
  geom_text(
    data = space_gender, 
    aes(x = 0.5, y = ymin + 0.025, 
        label = paste0(artist_gender, ' : ', round(space_ratio * 100, 2), ' %')),
    hjust = 0.5, family = f1, size = 2.5,
  ) +
  annotate(
    geom = 'text', x = 0.5, y = 0.945, hjust = 0.5, family = f2, size = 10,
    label = 'The pages of art history'
  ) +
  annotate(
    geom = 'text', x = 0.5, y = 0.85, 
    label = paste0(
      'What space did men and women in Janson’s History of Art and\n',
      'Gardner’s Art Through the Ages have on the pages based on\n',
      'area in centimeters squared of both the text and the figure by\n',
      'the area in centimeters squared of a single page'
      ),
    hjust = 0.5, family = f2, size = 4, lineheight = 0.75
  ) +
  theme_void() + 
  theme(
    panel.background = element_blank(),
    text = element_blank(),
    axis.ticks = element_blank()
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_03/plot.png"), 
  plot = p2, 
  width = 5, 
  height = 7,
  dpi = 300
)
