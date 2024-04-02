
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 14)
dubois <- janitor::clean_names(tuesdata$dubois_week10)

# Analysis ------------------------------------------------------------------------------------

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(
    data = dubois,
    aes(
      area = percentage,
      fill = occupation,
      label = paste0(percentage, '%', '\n', stringr::str_wrap(occupation, 10)),
    ),
  ) +
  treemapify::geom_treemap(
    color = 'black',
    show.legend = FALSE,
  ) +
  treemapify::geom_treemap_text(
    place = 'centre',
    color = 'black',
    size = 12,
  ) +
  scale_fill_manual(
    values = c(
      'Teachers' = '#c81c3c',
      'Ministers' = '#a5a2b2',
      'Government Service' = '#e2bdb0',
      'Business' = '#d3bca8',
      'Other Professions' = '#898a74',
      'House Wives' = '#e6b54e'
    ),
  ) +
  labs(
    title = 'Atlanta University students',
    subtitle = paste0(
      'The university was founded in 1867. It has instructed 6000 negro students.\n',
      'It has graduated 330 negroes among whom are:'
    ),
    caption = 'Source: Du Bois Challenge · Graphic: Matheus S. Rodrigues',
  ) +
  theme(
    plot.margin = margin(rep(10, 4)),
    plot.background = element_rect(
         fill = '#e4d2c1',
         color = NA,
    ),
    plot.title = element_text(
         family = f1,
         size = 25,
    ),
    plot.subtitle = element_text(
         family = f2,
         size = 11,
    ),
    plot.caption = element_text(
         color = 'grey40',
         hjust = 0,
    )
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_14/plot.png'),
  plot = p1,
  width = 6,
  height = 6,
  dpi = 300,
)
