
associates <- readRDS(here::here('2023/week_01/data.rds'))

library(ggplot2)
library(ggtext)
library(showtext)

showtext_auto()
font_add_google('Noto Sans', 'noto')

f1 <- 'noto'
p1 <- ggplot(associates) +
  geom_line(aes(x = date, y = associate, color = club), size = 1.1, show.legend = FALSE) +
  geom_segment(aes(x = as.Date('2022-12-10'), xend = as.Date('2022-12-31'),
                   y = 38000, yend = 38000),
               arrow = arrow(length = unit(0.025, 'npc'), type = 'closed', ends = 'both'), 
               color = '#13678A') +
  geom_text(aes(x = as.Date('2022-12-20'), y = 38000, label = 'Promotion in the value of the\nFortaleza associate'), 
            family = f1, color = '#13678A', size = 3.5, hjust = 'middle', check_overlap = TRUE) + 
  scale_x_date(date_breaks = '3 days', date_labels = '%d %b') +
  scale_y_continuous(breaks = seq(35000, 41000, 1000)) +
  scale_color_manual(values = c('#0F0F0F', '#C15143')) +
  labs(
    title = 'How did the associate numbers end?',
    subtitle = "Evaluation of the numbers of associates of <b style='color:#C15143'>Fortaleza</b> and <b style='color:#0F0F0F'>Ceará</b> in the month of December 2022",
    caption = 'Source: Fortaleza EC and Ceará SC · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = 'gray85', linetype = 'dotted'),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = f1, color = 'grey35', size = 18),
    plot.subtitle = element_markdown(family = f1, color = 'grey35'),
    plot.caption = element_text(family = f1, color = 'grey35', hjust = 0),
    axis.text = element_text(family = f1, color = 'gray50'),
    axis.ticks = element_line(color = 'gray85')
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_01/plot.png"), 
  plot = p1, 
  width = 9, 
  height = 6,
  dpi = 300
)
