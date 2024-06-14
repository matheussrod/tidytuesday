
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 24)
pride_index <- janitor::clean_names(tuesdata$pride_index)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Fredoka', 'fredoka')
sysfonts::font_add_google('Roboto', 'roboto')

f1 <- 'fredoka'
f2 <- 'roboto'

df_text <- data.frame(
  x = 1.25,
  y = c(42000, 40000),
  label = c(
    'Campus pride index',
    paste0(
      'Relationship between pride index and\n',
      'student population on US Campuses.\n',
      'The rating is from 1 to 5, with 5 being\n',
      'the best rating.'
    )
  ),
  hjust = 0,
  vjust = c(0, 1),
  color = '#d93240',
  size = c(9, 4),
  family = c(f1, f2),
  fontface = c('bold', 'plain')
)

p1 <- ggplot(pride_index, aes(x = rating, y = students)) +
  
  ## boxplot ----------------------------------------------------------------------------------
  geom_boxplot(
    aes(
      group = rating,
    ),
    fill = '#f2ecce',
    alpha = 0.5,
    outlier.colour = '#3dbcd9',
  ) +
  
  ## jitter -----------------------------------------------------------------------------------
  geom_jitter(
    width = 0.1,
    height = 0.1,
    color = '#3dbcd9',
    alpha = 0.5,
  ) +
  
  ## title and subtitle -----------------------------------------------------------------------
  geom_text(
    data = df_text,
    aes(
      x = x, 
      y = y,
      label = label,
      size = size,
      hjust = hjust,
      vjust = vjust,
      size = size,
      color = color,
      family = family,
      fontface = fontface,
    ),
  ) +
  
  ## scales and coords ------------------------------------------------------------------------
  scale_size_identity() +
  scale_color_identity() +
  scale_y_continuous(
    position = 'right',
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, 0.5),
    labels = seq(1, 5, 0.5),
  ) +
  labs(
    x = 'Rating',
    y = 'Students',
    caption = 'Source: Campus Pride Index · Graphic: Matheus S. Rodrigues',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    plot.margin = margin(rep(15, 4)),
    plot.background = element_rect(
         fill = '#f2ecce',
         color = NA,
    ),
    plot.caption = element_text(
         family = f2,
         color = 'grey40',
         size = 8,
         hjust = 0,
         margin = margin(t = 10),
    ),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = '#fdffd9'),
    panel.grid.minor = element_blank(),
    axis.text = element_text(
         family = f2,
    ),
    axis.title = element_text(
         family = f2,
         size = 8,
    ),
    axis.title.x = element_text(
         margin = margin(t = 10),
    ),
    axis.title.y.right = element_text(
         margin = margin(l = 10),
    ),
    axis.ticks = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_24/plot.png'),
  plot = p1,
  width = 7,
  height = 5,
  dpi = 300,
)
