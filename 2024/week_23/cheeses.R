
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- janitor::clean_names(tuesdata$cheeses)

# Functions -----------------------------------------------------------------------------------
extract <- function(x, pattern, group) {
  stringr::str_extract(string = x, pattern = pattern, group = group)
}

# Analysis ------------------------------------------------------------------------------------

pattern_percent <- '(\\d{1,2})\\-(\\d{1,2})'
pattern_g <- '(\\d{1,2})\\.?(\\d{1,2})? g'

fat_pct <- cheeses |> 
  dplyr::filter(!is.na(fat_content)) |> 
  dplyr::mutate(vegetarian = ifelse(is.na(vegetarian), FALSE, vegetarian)) |> 
  dplyr::mutate(
    fat_pct1 = as.integer(extract(fat_content, pattern_percent, 1)),
    fat_pct2 = as.integer(extract(fat_content, pattern_percent, 2)),
    fat_g1 = as.integer(extract(fat_content, pattern_g, 1)),
    fat_g2 = as.integer(stringr::str_pad(extract(fat_content, pattern_g, 2), 2, 'right', '0')) * 0.01,
    fat_g2 = ifelse(!is.na(fat_g1) & is.na(fat_g2), 0, fat_g2),
    fat_pct = (fat_pct1 + fat_pct2)/2,
    fat_g = fat_g1 + fat_g2,
    fat = dplyr::case_when(
      !is.na(fat_pct) ~ fat_pct,
      !is.na(fat_g) ~ fat_g,
      .default = as.double(stringr::str_remove(fat_content, '%'))
    )
  ) |> 
  dplyr::select(
    cheese,
    type,
    vegetarian,
    fat
  )

fat_per_cheese <- fat_pct |> 
  dplyr::group_by(cheese, vegetarian) |> 
  dplyr::reframe(fat = mean(fat)) |> 
  dplyr::mutate(fill = ifelse(vegetarian, '#f2f2f2', '#d95204'))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Roboto', 'roboto')

f1 <- 'outfit'
f2 <- 'roboto'

df_text <- data.frame(
  x = c(75, 67.5, 63),
  y = 20,
  label = c(
    'Fat in cheese', 
    "<span style='color:#f2f2f2;'>vegetarian</span> vs. <span style='color:#d95204;'>non vegetarian</span>", 
    paste0(
      '248 evaluated by cheese.com contain information<br>',
      'about the percentage of fat present in cheeses.<br>',
      'Below is shown what this percentage<br>',
      'is considering vegetarian and<br>',
      'non-vegetarian cheeses.'
    )
  ),
  size = c(12, 5, 3.5),
  fontface = c('bold', 'bold', 'plain'),
  family = c(f1, f2, f2)
)

p1 <- ggplot(fat_per_cheese) +
  
  ## geom col ---------------------------------------------------------------------------------
  geom_col(
    aes(
      x = forcats::fct_reorder(cheese, fat),
      y = fat,
      fill = fill,
    ),
    width = 1,
    show.legend = FALSE,
  ) +
  
  ## text: title and subtitle -----------------------------------------------------------------
  ggtext::geom_richtext(
    data = df_text,
    aes(
      y = x,
      x = y,
      label = label,
      size = size,
      fontface = fontface,
      family = family,
    ),
    hjust = 0,
    vjust = 1,
    fill = NA, 
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    show.legend = FALSE,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_fill_identity() +
  scale_y_continuous(
    breaks = seq(0, 60, 20),
    labels = paste0(seq(0, 60, 20), '%'),
  ) +
  scale_size_identity() +
  scale_x_discrete(
    expand = c(0, 5),
  ) +
  labs(
    x = NULL,
    y = NULL,
    caption = 'Source: cheese.com · Graphic: Matheus S. Rodrigues'
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme(
    text = element_text(family = f2,),
    plot.margin = margin(rep(30, 4)),
    plot.background = element_rect(
         fill = '#d9984a',
         color = NA,
    ),
    plot.caption = element_text(
         size = 8,
         color = 'grey20',
         margin = margin(t = 15),
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
          color = '#d27525',
    ),
    axis.ticks = element_blank(),
    axis.text.y = element_text(
         color = 'black',
         size = 8,
         margin = margin(t = 5),
    ),
    axis.text.x = element_blank(),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_23/plot.png'),
  plot = p1,
  width = 6,
  height = 6,
  dpi = 300,
)
