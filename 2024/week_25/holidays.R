
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 25)
federal_holidays <- janitor::clean_names(tuesdata$federal_holidays)

# Functions -----------------------------------------------------------------------------------
generate_sequence <- function(n) {
  if (n == 1) {
    return(c(0))
  } else {
    return(seq(from = -0.25 * (n - 1) / n, to = 0.25 * (n - 1) / n, length.out = n))
  }
}

# Analysis ------------------------------------------------------------------------------------
year_qty <- federal_holidays |> 
  dplyr::mutate(qty = dplyr::n(), .by = year_established) |> 
  dplyr::ungroup() |> 
  dplyr::distinct(year_established, qty)

years_points <- year_qty |> 
  dplyr::rowwise() |> 
  dplyr::mutate(points = list(generate_sequence(qty))) |> 
  tidyr::unnest(points)


# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Oswald', 'oswald')
sysfonts::font_add_google('Roboto', 'roboto')
sysfonts::font_add_google('Lato', 'lato')

f1 <- 'oswald'
f2 <- 'roboto'
f3 <- 'lato'

prop <- 10
df_plot <- years_points |> 
  dplyr::mutate(
    x = points,
    y = year_established,
    x_label = min(years_points$points) * prop/2,
    y_label = y,
    label = year_established
  )

limits_x <- c(min(df_plot$x), max(df_plot$x)) * prop

p1 <- ggplot(df_plot) +
  
  ## points -----------------------------------------------------------------------------------
  geom_point(
    aes(
      x = x,
      y = y,
    ),
    color = 'white',
  ) +
  
  ## labels -----------------------------------------------------------------------------------
  geom_text(
    aes(
      x = x_label,
      y = y_label,
      label = label,
    ),
    color = 'white',
    size = 2.5,
    family = f3,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_y_reverse() +
  scale_x_continuous(
    limits = limits_x,
  ) +

  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'US Federal Holidays timeline',
    subtitle = paste0(
      'The year in which holidays was officially\n',
      'established as a federal holiday'
    ),
    caption = 'Source: Wikipedia\nGraphic: Matheus S. Rodrigues',
  ) +
  
  ## theme ------------------------------------------------------------------------------------
  theme_void() +
  theme(
    text = element_text(
         family = f2,
         color = 'white',
    ),
    plot.margin = margin(rep(15, 4)),
    plot.background = element_rect(
         fill = 'black',
         color = 'white',
    ),
    plot.title = element_text(
         family = f1,
         face = 'bold',
         size = 13,
         hjust = 0.5,
    ),
    plot.subtitle = element_text(
         size = 8,
         hjust = 0.5,
    ),
    plot.caption = element_text(
         size = 6,
         hjust = 0.5,
         margin = margin(t = 10),
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_25/plot.png'),
  plot = p1,
  width = 3,
  height = 6,
  dpi = 300,
)
