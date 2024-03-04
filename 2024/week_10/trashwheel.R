
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 10)
trashwheel  <- tuesdata$trashwheel

# Analysis ------------------------------------------------------------------------------------
imgs_path <- here::here('2024/week_10/imgs')
df_imgs <- dplyr::tibble(img_path = list.files(imgs_path, full.names = TRUE)) |> 
  dplyr::mutate(id = stringr::str_extract(img_path, '[^/]+(?=\\.\\w+$)'))

podium <- trashwheel |> 
  janitor::clean_names() |> 
  dplyr::group_by(id, name) |> 
  dplyr::reframe(
    records = dplyr::n(),
    weight = sum(weight, na.rm = TRUE),
    weight_prop = weight / records
  ) |> 
  dplyr::arrange(dplyr::desc(weight_prop)) |> 
  dplyr::mutate(
    x = c(2, 1, 3, 4),
    y = weight_prop,
    y_img = y + c(1.65, rep(1.5, 3))
  ) |> 
  dplyr::left_join(df_imgs, by = 'id') |> 
  dplyr::mutate(
    label = paste0(
      '(', 1:4, 'º): ',
      as.character(round(weight_prop, 1)), ' ton / dumpster<br>',
      "<span style='font-size:8pt;'>", stringr::str_wrap(name, 15), '</span>'
    )
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(podium) + 
  geom_col(aes(x = x, y = y), width = 1, fill = '#7dc24e', color = 'white') + 
  ggtext::geom_richtext(
    aes(x = x, y = y + 0.1, label = label),
    size = 3, vjust = 0, family = f1, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  ggimage::geom_image(aes(x = x, y = y_img, image = img_path), size = 0.225) +
  scale_x_continuous(breaks = 1:4, labels = podium$name[podium$x]) +
  ylim(c(0, max(podium$y_img) + 0.5)) +
  labs(
    title = 'The most voracious eater in Mr. Trash\nWheel\'s family',
    subtitle = paste0(
      'Mr. Trash Wheel is a semi-autonomous trash interceptor that utilizes a unique combination ',
      'of solar\nand hydro power to remove hundreds of tons of trash from the water annually. ',
      'The Mr. Trash Wheel\nfamily currently consists of four members: Mr. Trash Wheel, ',
      'Professor Trash Wheel, Captain Trash\nWheel, and Gwynnda the Good Wheel of the West.\n\n',
      
      'Their primary role is to "eat" the garbage present in rivers, streams, or other outfalls. ',
      'So, the\nquestion remains: who in the family consumes the most?'
    ),
    caption = 'Source: Mr. Trash Wheel · Graphic: Matheus S. Rodrigues',
    x = NULL, y = NULL
  ) +
  theme(text = element_text(family = f2),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = f1, face = 'bold', color = '#0678bf', size = 25),
    plot.subtitle = element_text(size = 9.5, margin = margin(b = 20)),
    plot.caption = element_text(color = 'grey40',size = 8,margin = margin(t = 10),hjust = 0),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_10/plot.png'),
  plot = p1,
  width = 7,
  height = 8,
  dpi = 300
)
