
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 4)
english_education  <- tuesdata$english_education

# Analysis ------------------------------------------------------------------------------------
size_income <- english_education |> 
  dplyr::select(size_flag, income_flag) |>
  dplyr::mutate(size_flag = dplyr::case_when(
    size_flag == 'Small Towns' ~ 'Small towns',
    size_flag == 'Medium Towns' ~ 'Medium towns',
    size_flag == 'Large Towns' ~ 'Large towns',
    .default = 'Cities'
  )) |>
  tidyr::drop_na() |> 
  dplyr::mutate(income_flag = ifelse(income_flag == 'Cities', 'Higher deprivation towns', income_flag)) |> 
  dplyr::mutate(income_flag = factor(
    income_flag,
    levels = c('Higher deprivation towns', 'Mid deprivation towns', 'Lower deprivation towns'), 
    ordered = TRUE
  )) |> 
  dplyr::mutate(income_flag = forcats::fct_rev(income_flag)) |> 
  dplyr::group_by(size_flag, income_flag) |> 
  dplyr::count()

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Open Sans', 'open')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 100)

f1 <- 'open'
colors = c(
  'Higher deprivation towns' = '#871A5B',
  'Mid deprivation towns' = '#D3D3D3',
  'Lower deprivation towns' = '#206095'
)

p1 <- ggplot(size_income, aes(x = size_flag, y = n, fill = income_flag, color = income_flag)) + 
  geom_hline(yintercept = 0, linetype = 1, color = '#b3b3b3', linewidth = 1.25) +
  geom_bar(position = 'fill', stat = 'identity', key_glyph = draw_key_point, width = 0.7) + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20), expand = c(0, 0)) +
  coord_flip() + 
  labs(
    title = 'Small towns are less likely to be classed as having high income\ndeprivation',
    subtitle = 'Income deprivation group by town size, England',
    caption = paste0(
      'Source: Office for National Statistics analysis using Longitudinal',
      ' Educational Outcomes (LEO)\nfrom the Department for Education (DfE)'
    ), x = NULL, y = NULL
  ) +
  guides(
    color = guide_legend(
      title = NULL,
      label.position = 'right',
      override.aes = list(size = 3),
      direction = 'horizontal',
      reverse = TRUE
    ),
    fill = FALSE
  ) +
  theme(
    text = element_text(family = f1),
    plot.margin = margin(rep(20, 4)),
    plot.title = element_text(size = 10, face = 'bold', margin = margin(b = 13)),
    plot.subtitle = element_text(size = 8.5, margin = margin(b = 30)),
    plot.caption = element_text(color = '#707070', hjust = 0, margin = margin(t = 10), lineheight = 1.2),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    panel.background = element_blank(),
    panel.grid.major = element_line(color = '#d9d9d9'),
    axis.text = element_text(size = 8),
    legend.position = c(-0.225, 1.105),
    legend.justification = 'left',
    legend.key = element_rect(fill = 'transparent'),
    legend.text = element_text(size = 8, color = '#707070', margin = margin(l = -3, r = 20)),
    axis.ticks = element_blank()
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_04/plot.png'),
  plot = p1,
  width = 6,
  height = 4,
  dpi = 300
)

  