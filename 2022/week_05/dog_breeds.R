

# Sources -------------------------------------------------------------------------------------------

# https://www.akc.org/
# https://www.usatoday.com/picture-gallery/life/2021/06/28/the-50-most-popular-dog-breeds-in-america/45134329/
# https://www.vox.com/2016/8/31/12715176/most-popular-dog-breeds


# Main questions ------------------------------------------------------------------------------------

# - What characteristics are best evaluated?


# Dog breeds data -----------------------------------------------------------------------------------

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_characteristic <-  breed_traits |> 
  dplyr::select(-`Coat Type`, -`Coat Length`) |> 
  tidyr::pivot_longer(cols = -1, names_to = "characteristic") |> 
  dplyr::group_by(characteristic) |> 
  dplyr::summarise(value = sum(value),
                   percent = value / (195*5)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(percent))


breed_details <- breed_characteristic |> 
  dplyr::left_join(trait_description[, 1:3], by = c("characteristic" = "Trait"))

# Inspirations --------------------------------------------------------------------------------------


# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)
library(ggchicklet)
library(showtext)

font_add_google("Ubuntu Mono", "Ubuntu Mono")

f1 <- "Ubuntu Mono"
f3 <- "Ubuntu Mono"

showtext_auto()

plot <- ggplot(breed_details, aes(forcats::fct_reorder(characteristic, percent), y = percent)) + 
  geom_chicklet(aes(y = 1), radius = grid::unit(0.75, 'lines'), fill = "white", color = "grey90") + 
  geom_chicklet(radius = grid::unit(0.75, 'lines'), fill = "#201E41", color = "grey90") + 
  geom_text(aes(label = paste0(round(percent*100, 1), " %")), family = f3, hjust = 1.2, color = "white", size = 12) +
  
  geom_text(aes(y = 0.02, label = stringr::str_wrap(Trait_1, width = 15)), family = f3, hjust = 0, color = "white", size = 9, lineheight = 0.35) +
  geom_text(aes(y = 1.02, label = stringr::str_wrap(Trait_5, width = 15)), family = f3, hjust = 0, color = "black", size = 9, lineheight = 0.35) +
  
  coord_flip(ylim = c(0, 1.2)) +
  labs(
    title = "Breed characteristics",
    subtitle = "For each characteristic, 195 breeds were evaluated. The percentage represents, for the analyzed characteristic, 
how close the evaluated breeds came to achieving the best evaluation.",
    caption = "Source: American Kennel Club · Graphic: Matheus S. Rodrigues",
    x = "", y = ""
  ) +
  theme(
    plot.margin = margin(rep(20, 4)),
    panel.background = element_blank(),
    plot.title = element_text(family = f1, face = "bold", size = 120),
    plot.subtitle = element_text(family = f3, size = 35, hjust = 0, margin = margin(t = 5, b = 20), lineheight = 0.35),
    plot.caption = element_text(family = f3, color = "grey40", size = 30, hjust = 1),
    plot.title.position = "plot",
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = f3, size = 35, margin = margin(r = -10)),
    axis.text.x = element_blank()
  )

ggsave(filename = here::here("2022/week_05/plot.png"), plot = plot, width = 10, height = 8, units = "in", dpi = 320)

