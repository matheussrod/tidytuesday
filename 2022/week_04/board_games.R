

# Sources -------------------------------------------------------------------------------------------

# https://boardgamegeek.com/
# https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews


# Main questions ------------------------------------------------------------------------------------

# Does the maximum and minimum play time decrease over time?
# Is play time related to the minimum age?


# Board games data ----------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

details <- tuesdata$details
ratings <- tuesdata$ratings

details_time <- details |> 
  dplyr::group_by(year = yearpublished) |> 
  dplyr::summarise(min_play = mean(minplaytime),
                   quantity = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(year >= 2001, year <= 2022)


# Inspirations --------------------------------------------------------------------------------------

#https://pin.it/2Tb8WDd


# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)
library(showtext)


font_add_google("MedievalSharp", "MedievalSharp")
font_add_google("Germania One", "Germania One")

f1 <- "MedievalSharp"
f2 <- "Germania One"

showtext_auto()

plot <- 
ggplot(details_time) +
  geom_segment(aes(x = 0, xend = round(min_play, 0), y = year, yend = year), size = 0.35, color = "#90675c") +
  geom_point(aes(x = round(min_play, 0), y = year), size = 6.5, shape = 1, color = "#5e422f") + 
  geom_segment(aes(x = 0, xend = 0, y = min(year) - 1, yend = max(year) + 1), color = "#90675c") +
  geom_point(aes(x = round(min_play, 0), y = year, size = quantity), color = "#90675c") + 
  geom_text(aes(x = min_play, y = year, label = round(min_play, 0)), family = f2, size = 10, hjust = -1.5) +
  geom_curve(aes(yend = 2021.5, xend = 58, y = 2018, x = 65), arrow = arrow(length = unit(0.5, "lines")), curvature = 0.15, color = "#5e422f") +
  geom_text(aes(x = 65, y = 2017.5, label = "Average minimum time"), color = "#5e422f", size = 10) +
  scale_y_reverse(breaks = details_time$year, labels = details_time$year) +
  labs(
    title = "How much play time?",
    subtitle = "Analysis of board games launched in the 21st century. For each year, the average
minimum time of board games was obtained. The size of the circle represents the
number of games released in the respective year.",
    caption = "Source: Board Games Geek · Graphic: Matheus S. Rodrigues",
    x = "", y = "", size = "Quantity of\nboard games"
  ) + 
  theme(
    panel.background = element_blank(),
    plot.margin = unit(rep(1.5, 4), "lines"),
    plot.title = element_text(family = f2, face = "bold", color = "#5e422f", size = 120),
    plot.subtitle = element_text(family = f1, size = 35, margin = margin(b = 10), lineheight = 0.3),
    plot.caption = element_text(family = f2, size = 30, color = "grey40", hjust = 0, margin = margin(t = -10)),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = f2, color = "#5e422f", size = 25, margin = margin(r = -15)),
    axis.ticks = element_blank(),
    legend.title = element_text(family = f2, size = 35, lineheight = 0.3, margin = margin(b = -10)),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(family = f2, size = 30)
  )

ggsave(filename = here::here("2022/week_04/plot.png"), plot = plot, width = 8, height = 8, units = "in", dpi = 320)

