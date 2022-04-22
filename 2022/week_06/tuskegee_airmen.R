

# Sources -------------------------------------------------------------------------------------------

# https://commemorativeairforce.org/
# https://en.wikipedia.org/wiki/Tuskegee_Airmen


# Main questions ------------------------------------------------------------------------------------


# Tuskegee Airmen data ------------------------------------------------------------------------------

library(stringr)

tuesdata <- tidytuesdayR::tt_load(2022, week = 6)
airmen <- tuesdata$airmen

#Sys.getlocale("LC_TIME")
airmen_jets <- airmen |> 
  dplyr::filter(!is.na(aerial_victory_credits)) |> 
  dplyr::select(name, victory_cred = aerial_victory_credits) |> 
  tidyr::separate_rows(victory_cred, sep = ";") |> 
  tidyr::separate_rows(victory_cred, sep = "and") |> 
  dplyr::mutate(victory_cred = str_squish(victory_cred),
                jet = ifelse(str_detect(victory_cred, "on"), str_extract(victory_cred, "(?<=\\d\\s).*(?=[\\s]on)"), str_extract(victory_cred, "(?<=\\d\\s).*$")),
                jet_n = ifelse(str_detect(victory_cred, "Downed"), str_extract(victory_cred, "(?<=Downed\\s)[1-3\\/\\s]*(?=[\\s])"), str_extract(victory_cred, "[1-3\\/]*(?=[\\s])")),
                victory_dt = as.Date(str_extract(victory_cred, "(?<=on) .*$"), format = " %B %d, %Y")) |> 
  tidyr::fill(victory_dt, .direction = "up")

airmen_jets$jet <- stringr::str_remove(airmen_jets$jet, "1/2 ")
airmen_jets$jet_n <- as.integer(with(airmen_jets, ifelse(jet_n == "1/2", "2", ifelse(jet_n == "1 1/2", 2, jet_n))))

airmen_jets_n <- airmen_jets |> 
  dplyr::mutate(group_dt = paste0(lubridate::year(victory_dt), "-", lubridate::month(victory_dt), "-", "01")) |> 
  dplyr::group_by(group_dt) |> 
  dplyr::summarise(jet_n = sum(jet_n)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(group_dt = as.Date(group_dt, format = "%Y-%m-%d")) |> 
  dplyr::arrange(group_dt, jet_n)

airmen_names <- airmen_jets |> 
  dplyr::select(name) |> 
  dplyr::distinct() |> 
  dplyr::mutate(x = rep(1:8, 9), y = rep(1:8, each = 9))

airmen_xy <- airmen_names |> 
  dplyr::filter(x == y) |> 
  dplyr::group_by(x, y) |> 
  dplyr::mutate(y = y + c(0.3, -0.3))


# Inspirations --------------------------------------------------------------------------------------


# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)
library(showtext)
library(patchwork)

font_add_google("Black Ops One", "Black Ops")
font_add_google("Allerta Stencil", "Allerta")

f1 <- "Black Ops"
f2 <- "Allerta"

showtext_auto()

plot_names <- ggplot() + 
  geom_text(data = dplyr::filter(airmen_names, x != y), aes(x = x, y = y, label = name), family = f2, color = "grey40", size = 8) + 
  geom_text(data = airmen_xy, aes(x = x, y = y, label = name), family = f2, color = "grey40", size = 8) + 
  xlim(c(0.5, 8.55)) + ylim(c(0.5, 8.55)) +
  theme_void()

plot <- ggplot(airmen_jets_n) + 
  geom_segment(aes(x = group_dt, xend = group_dt, y = 0.0065, yend = -0.05), color = "grey90", linetype = "dashed") +
  geom_point(aes(x = group_dt, y = -0.01, size = jet_n), color = "#262626", alpha = 0.50, show.legend = FALSE) + 
  geom_text(aes(x = group_dt, y = 0.008, label = jet_n), family = f2, size = 10) +
  labs(
    title = "Jets shot down by Tuskegee Airmen in World War II",
    subtitle = "In tribute of",
    caption = "Source: CAF · Graphic: Matheus S. Rodrigues",
    x = "", y = ""
  ) +
  ylim(c(-0.05, 0.05)) +
  scale_radius(range = c(5, 50)) +
  scale_x_date(breaks = airmen_jets_n$group_dt, labels = airmen_jets_n$group_dt, date_labels = "%m/%y") +
  theme(
    plot.margin = margin(rep(20, 4)),
    panel.background = element_blank(),
    plot.title = element_text(family = f1, size = 80, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = f2, color = "grey40", size = 50, hjust = 0.5, margin = margin(t = 20, b = -25)),
    plot.caption = element_text(family = f2, size = 30, color = "grey40", hjust = 0, margin = margin(t = 10)),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = f2, size = 25, margin = margin(t = -20))
  )

plot2 <- plot + inset_element(plot_names, left = 0, bottom = 0.65, right = 1, top = 0.93)
ggsave(filename = here::here("2022/week_06/plot.png"), plot = plot2, width = 10, height = 8, units = "in", dpi = 320)
