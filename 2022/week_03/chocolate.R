

# Sources -------------------------------------------------------------------------------------------

# http://flavorsofcacao.com/chocolate_database.html
#  Rating Scale
# 
#  - 4.0 - 5.0  = Outstanding
#  - 3.5 - 3.9  = Highly Recommended
#  - 3.0 - 3.49 = Recommended
#  - 2.0 - 2.9  = Disappointing
#  - 1.0 - 1.9  = Unpleasant


# Main questions ------------------------------------------------------------------------------------

# - What most memorable characteristics are present in the best ratings?


# Chocolate data ------------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 3)
chocolate <- tuesdata$chocolate

chocolate_characteristics <- chocolate |> 
  dplyr::select(ref, cocoa_percent, characteristics = most_memorable_characteristics, rating) |> 
  dplyr::mutate(cocoa_percent = as.double(stringr::str_remove(cocoa_percent, "%"))/100,
                characteristics = stringr::str_to_title(characteristics)) |> 
  tidyr::separate_rows(characteristics, sep = ", ")

chocolate_ratings <- dplyr::mutate(
    chocolate_characteristics, 
    rating_category = dplyr::case_when(
      rating < 2 ~ "Unpleasant",
      rating < 3 ~ "Disappointing",
      rating < 3.5 ~ "Recommended",
      rating < 4 ~ "Highly Recommended",
      TRUE ~ "Outstanding")) |> 
  dplyr::mutate(rating_category = factor(rating_category, levels = c("Unpleasant", "Disappointing", "Recommended", "Highly Recommended", "Outstanding")))

chocolate_top10 <- chocolate_ratings |> 
  dplyr::group_by(rating_category, characteristics) |> 
  dplyr::summarise(quantity = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(rating_category, dplyr::desc(quantity)) |> 
  dplyr::group_by(rating_category) |> 
  dplyr::mutate(row = dplyr::row_number()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(row <= 10)

chocolate_percent <- chocolate_top10 |> 
  dplyr::group_by(rating_category) |> 
  dplyr::mutate(percent = quantity / sum(quantity)) |> 
  dplyr::ungroup()


# Inspirations --------------------------------------------------------------------------------------

# [The 34 Best Interactive Data Visualizations from the New York Times - Dolphins](https://pin.it/2iyqmiU)


# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)
library(showtext)

sysfonts::font_add_google("Righteous", "Righteous")
sysfonts::font_add_google("Baloo 2", "Baloo")
sysfonts::font_add_google("Changa", "Changa")

showtext_auto()

f1 <- "Righteous"
f2 <- "Baloo"
f3 <- "Changa"

plot <- ggplot(data = chocolate_percent) +
  geom_point(aes(x = rating_category, y = row, size = percent, color = quantity)) + 
  geom_text(aes(x = rating_category, y = row, label = characteristics), vjust = -1.5, size = 12, stat = "unique", family = f3) +
  scale_y_reverse() +
  scale_color_gradient2(low = "#eef3e5", mid = "#a87b6a", high = "#1e1517") +
  labs(
    title = "Most memorable characteristics", 
    subtitle = "2530 chocolates were rated on a scale of 1 to 5. This scale was transformed with the following criteria:
4.0 - 5.0 = Excellent; 3.5 - 3.9 = Highly recommended; 3.0 - 3.49 = Recommended; 2.0 - 2.9 = Disappointing;
1.0 - 1.9 = Unpleasant. For each category, it was evaluated which characteristics were most memorable", 
    caption = "Source: Flavors of Cacao · Graphic: Matheus S. Rodrigues",
    size = "Percent\nper category", color = "Quantity\nper category",
    x = "", y = "") +
  theme(
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20),
    panel.background = element_blank(),
    plot.title = element_text(family = f1, face = "bold", color = "brown", size = 120),
    plot.subtitle = element_text(family = f2, color = "#1e1517", size = 40, margin = margin(b = 25), lineheight = 0.3),
    plot.caption = element_text(family = f1, color = "grey40", size = 30, margin = margin(t = 20), hjust = 0),
    legend.title = element_text(family = f3, size = 40, lineheight = 0.25, margin = margin(b = -12.5)),
    legend.spacing = unit(2, "lines"),
    legend.text = element_text(family = f3, size = 30, margin = margin(l = -10)),
    legend.key = element_rect(fill = "transparent"),
    legend.position = "right",
    legend.background = element_rect(fill = "transparent"),
    axis.text.x = element_text(family = f3, face = "plain", colour = "grey10", size = 40),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
  )

ggsave(filename = here::here("2022/week_03/plot.png"), plot = plot, width = 10, height = 8, units = "in", dpi = 320)
