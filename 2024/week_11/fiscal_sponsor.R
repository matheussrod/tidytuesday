
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 11)
fiscal_sponsor_directory  <- tuesdata$fiscal_sponsor_directory

# Analysis ------------------------------------------------------------------------------------
fiscal_project <- fiscal_sponsor_directory |> 
  dplyr::filter(!is.na(fiscal_sponsorship_model)) |> 
  tidyr::separate_longer_delim(cols = project_types, delim = '|') |> 
  dplyr::select(name, year_501c3, n_sponsored, project_types)

fiscal_project_cumsum <- fiscal_project |> 
  dplyr::count(year_501c3, project = project_types, name = 'qty') |>
  dplyr::arrange(year_501c3) |> 
  dplyr::group_by(project) |> 
  dplyr::mutate(qty_cumsum = cumsum(qty)) |> 
  dplyr::ungroup() |> 
  dplyr::select(year = year_501c3, project, qty_cumsum) |> 
  dplyr::filter(year >= 1950)

decades_rect <- data.frame(
  xmin = 0,
  xmax = max(fiscal_project_cumsum$qty_cumsum),
  ymin = 1950 + seq(0, 60, 20),
  ymax = 1960 + seq(0, 60, 20)
)

df_title <- data.frame(
  x = 55, 
  y = 1970,
  label = paste0(
    "<b><span style='color:#3382b1;'>EDUCATION PROJECTS</span></b> FROM",
    '<br>FISCAL SPONSOR DIRECTORY'
  )
)

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Jost', 'jost')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'jost'

p1 <- ggplot(fiscal_project_cumsum) +
  geom_rect(
    data = decades_rect,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = '#fef0e1'
  ) +
  geom_step(
    aes(
      y = year, 
      x = qty_cumsum, 
      group = project, 
    ),
    color = 'grey80',
    show.legend = FALSE,
  ) +
  geom_step(
    data = dplyr::filter(
      fiscal_project_cumsum,
      project == 'Education'
    ),
    aes(
      y = year, 
      x = qty_cumsum, 
      group = project, 
    ), 
    color = '#3382b1',
    linewidth = 0.8,
    show.legend = FALSE,
  ) +
  ggtext::geom_richtext(
    data = df_title,
    aes(
      x = x,
      y = y,
      label = label,
    ),
    label.padding = grid::unit(rep(0, 4), 'pt'), 
    label.color = NA,
    fill = NA,
    hjust = 0,
    vjust = 0,
    family = f1,
    size = 7,
    lineheight = 1.2,
  ) +
  annotate(
    'text',
    x = 55,
    y = 1987,
    label = paste0(
      'Fiscal sponsorship is a business model that exponentially\nexpands the charitable work ',
      'of the nonprofit sector efficiently\nand cost-effectively.\n',
      'Education projects are among the most supported, second in\nquantity only to the Arts ',
      'and Culture category.'
    ),
    hjust = 0,
    vjust = 0,
    family = f1,
    size = 3.5,
    lineheight = 1.2,
  ) +
  scale_y_reverse(
    breaks = seq(1955, 2015, 10),
    labels = c('1950', '60\'', '70\'', '80\'', '90\'', '2000', '10\''),
    expand = c(0, 1)
  ) +
  scale_x_continuous(expand = c(0, 1)) +
  labs(
    x = 'Number of projects',
    y = NULL,
    caption = 'Source: Fiscal Sponsor Directory · Graphic: Matheus S. Rodrigues',
  ) +
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(
         fill = '#f4e7d7',
         color = NA,
    ),
    plot.caption = element_text(
         family = f1,
         color = 'grey50',
         margin = margin(t = 10),
         hjust = 0,
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(
         family = f1,
    ),
    axis.title = element_text(
         family = f1,
         size = 9,
         color = 'grey30',
    ),
    axis.title.x = element_text(
         margin = margin(t = 10),
    )
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_11/plot.png'),
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)
