
library(ggplot2)
library(showtext)

# European Drug Development -----------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 11)
drugs <- tuesdata$drugs

perc_pills <- drugs |> 
  dplyr::select(
    category, generic, biosimilar, orphan_medicine, exceptional_circumstances
  ) |> 
  tidyr::pivot_longer(cols = -1) |> 
  dplyr::group_by(category, name) |> 
  dplyr::summarise(perc = sum(value) / dplyr::n()) |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(name = forcats::fct_reorder(name, perc))

# Plot --------------------------------------------------------------------
showtext_auto()
font_add_google('Noto Sans', 'noto')
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'
f2 <- 'noto'

configs <- list(
  y1 = 0, y2 = 4, 
  ymin = 0.5, ymax = 2,
  xmin = 0.3, d = 0.8
)
bars_x <- cumsum(c(configs$y1, rep(3, 3)))

pills_plot <- perc_pills |>
  dplyr::mutate(
    category_desc = dplyr::case_when(
      name == 'biosimilar' ~ paste0(
        'The drug might be a biosimilar,\n',
        'meaning it\'s a biological medicine\n',
        'that\'s very similar to another one\n',
        'already approved and called the\n',
        'reference medicine.'
      ),
      name == 'exceptional_circumstances' ~ paste0(
        'The medicine may be authorized under\n',
        'exceptional circumstances if the\n',
        'applicant cannot provide enough\n',
        'data on its efficacy and safety.'
      ),
      name == 'generic' ~ paste0(
        'Generic medicine is the same as an\n',
        'authorized reference medicine,\n',
        'containing the same active\n',
        'substance(s) and used at the same\n',
        'dose(s) to treat the same disease(s).'
      ),
      name == 'orphan_medicine' ~ paste0(
        'Medicine that was created to treat\n',
        'a rare, serious, or chronic condition\n',
        'or for economic reasons, and might\n',
        'not have been developed without\n',
        'incentives.'
      )
    )
  ) |> 
  dplyr::mutate(
    perc_rcl = scales::rescale(1 - perc, c(configs$ymin, configs$ymax), c(0, 1)),
    xmin = c(bars_x + configs$xmin, bars_x + configs$xmin + configs$d),
    xmax = xmin + configs$d - 0.1,
    ymin = configs$ymin,
    ymax = perc_rcl,
    name = stringr::str_to_sentence(stringr::str_replace(name, '_', ' ')),
    name = stringr::str_wrap(name, width = 20),
    x_md = (xmin + xmax)/2
  ) |> 
  dplyr::mutate(n_breaks = stringr::str_count(name, '\\\n'))

p1 <- ggplot(pills_plot) + 
  ggchicklet::geom_rrect(
    data = data.frame(
      xmin = bars_x,
      xmax = bars_x + 2.5,
      ymin = configs$y1, ymax = configs$y2
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), size = 0.25,
    fill = '#72a7a2', color = 'black', radius = unit(0.1, 'npc'),
  ) +
  ggchicklet::geom_rrect(
    data = data.frame(
      xmin = bars_x + 0.1,
      xmax = bars_x + 2.4,
      ymin = configs$y1 + 0.1, ymax = configs$y2 - 0.1
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), size = 0.25,
    fill = 'white', color = 'black', radius = unit(0.1, 'npc')
  ) +
  ggchicklet::geom_rrect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    fill = '#d24326', color = NA, radius = unit(0.5, 'npc')
  ) +
  annotate('text',
    x = bars_x + 0.3, y = configs$y2 - 0.4, 
    label = paste0('Non-', unique(pills_plot$name)), 
    hjust = 0, vjust = 1, family = f1, size = 3.5, lineheight = unit(0.75, 'npc'),
    check_overlap = TRUE
  ) +
  geom_text(
    aes(x = x_md, y = ymin - 0.1, label = category), hjust = 0.5,
    family = f1, size = 2
  ) +
  geom_text(
    aes(x = xmin, y = configs$y2 - 0.7 - (0.2 * n_breaks), label = category_desc),
    family = f2, size = 2, hjust = 0, vjust = 1, fontface = 'italic',
    check_overlap = TRUE
  ) +
  geom_text(
    aes(
      x = x_md, y = perc_rcl + 0.1, 
      label = scales::percent_format()(1 - perc)
    ),
    family = f2, size = 2
  ) +
  annotate('text',
    x = 0, y = 4.5,
    label = paste0(
      'Understand the percentage of different types of medications among ',
      'human and veterinary animals in Europe, including biosimilars,\n',
      'medications for exceptional circumstances, generics, and orphan drugs. ',
      'The displayed percentages on the graph indicate the relative\n',
      'prevalence of medications that fall outside of each respective category. ',
      'For example, the analysis of generic medications showcases the\n',
      'proportion of medications that do not conform to the generic ',
      'classification.'
    ),
    family = f2, size = 3, hjust = 0, lineheight = unit(0.8, 'npc')
  ) + 
  labs(
    title = 'Drugs used in human and veterinary animals in Europe',
    caption = 'Source: European Medicines Agency · Graphic: Matheus S. Rodrigues'
  ) +
  coord_fixed(clip = 'off') + 
  theme_void(base_family = f1) + 
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(l = 50, r = 50, t = 20, b = 20),
    plot.title = element_text(size = 15, hjust = 0.11, margin = margin(b = 10)),
    plot.caption = element_text(size = 6, hjust = 0.07)
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_11/plot.png"),
  plot = p1, 
  width = 10, 
  height = 5,
  dpi = 300
)
