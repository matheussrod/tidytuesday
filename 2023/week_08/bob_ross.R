
library(dplyr)
library(ggplot2)
library(showtext)

# Bob Ross Paintings ------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 8)
bob_ross <- tuesdata$bob_ross

colors_stack <- bob_ross |> 
  select(season, color_hex) |> 
  mutate(
    color_hex = stringr::str_replace_all(color_hex, "[\\[\\]\\s\\']", ''),
    color_hex = stringr::str_split(color_hex, ',')
  ) |> 
  tidyr::unnest(color_hex) |> 
  group_by(season) |> 
  mutate(yax = row_number())

# Plot --------------------------------------------------------------------
showtext_auto()
font_add_google('Finger Paint', 'finger')
font_add_google('Patrick Hand', 'hand')
f1 <- 'finger'
f2 <- 'hand'

background <- 'white'

p1 <- ggplot(colors_stack) +
  geom_bin_2d(
    aes(x = season, y = yax, fill = color_hex, group = color_hex), 
    alpha = 0.75, color = '#262626', position = 'stack', stat = 'identity', 
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  coord_polar() + 
  ylim(c(-1000, NA)) +
  labs(
    title = 'Bob Ross palette',
    subtitle = 'An overview of the colors used by Bob Ross in 31 seasons',
    caption = 'Source: Bob Ross Colors data package · Graphic: Matheus S. Rodrigues'
  ) +
  theme_void(base_family = f1) +
  theme(
    panel.background = element_rect(fill = background, color = NA),
    plot.background = element_rect(fill = background, color = NA),
    plot.margin = margin(t = 40, b = 30),
    plot.title = element_text(size = 25, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(
      family = f2, size = 12, hjust = 0.5, margin = margin(b = -60)
    ),
    plot.caption = element_text(
      family = f2, color = 'grey30', size = 12, hjust = 0.5, 
      margin = margin(t = -40),
    )
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_08/plot.png"),
  plot = p1,
  width = 10,
  height = 10,
  dpi = 300
)
