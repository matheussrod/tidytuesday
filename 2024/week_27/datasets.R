
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 27)
tt_datasets <- janitor::clean_names(tuesdata$tt_datasets)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(tt_datasets) +
  
  ## point ------------------------------------------------------------------------------------
  geom_point(
    aes(
      x = variables,
      y = log(observations),
    ),
    color = '#162338',
    size = 3,
    alpha = 0.15,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_x_continuous(
    breaks = seq(0, 125, 25),
    labels = seq(0, 125, 25),
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'TidyTuesday datasets',
    subtitle = 'Number of variables x number of rows',
    caption = 'Source: {{ttmeta}} · Graphic: Matheus S. Rodrigues',
    x = 'Number of columns',
    y = 'log(Number of rows)'
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(
         family = f2,
    ),
    plot.margin = margin(rep(20, 4)),
    plot.title.position = 'plot',
    plot.title = element_text(
         family = f1,
         size = 20,
         face = 'bold',
         color = '#162338',
         hjust = 0.5,
    ),
    plot.subtitle = element_text(
         size = 10,
         color = '#162338',
         hjust = 0.5,
    ),
    plot.caption = element_text(
         size = 6,
         hjust = 0.5,
         margin = margin(t = 10),
         color = 'grey40',
    ),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(
         size = 7,
         color = 'grey30',
    ),
    axis.title.x = element_text(
         margin = margin(t = 5),
    ),
    axis.text = element_text(
         size = 6,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_27/plot.png'),
  plot = p1,
  width = 4,
  height = 5,
  dpi = 300,
)
