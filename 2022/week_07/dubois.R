

# Sources -------------------------------------------------------------------------------------------

# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022

dubois <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv")

dubois <- dubois |> 
  dplyr::mutate(Free_limit = ifelse(Free == 100, 3, Free),
                Free_label = ifelse(dplyr::row_number() %in% c(1, nrow(dubois)), paste0(Free, " %"), as.character(Free)))

year_trick <- 1863
dubois_new_line <- data.frame(
  "Year" = year_trick,
  Slave = 97,
  Free = 3,
  Free_limit = 3,
  Free_label = ""
)

dubois_trick <- rbind(dubois, dubois_new_line)

years_breaks <- seq(min(dubois$Year), max(dubois$Year), by = 0.5)
values <- runif(n = length(years_breaks), min = 2.85, max = 3.05)
df <- data.frame("year" = years_breaks, "value" = values)

dubois_breaks <- dubois_trick |> 
  dplyr::left_join(df, by = c("Year" = "year")) |> 
  dplyr::mutate(value = ifelse(Year > year_trick - 1, value, Free_limit))

dubois_lead <- dubois_trick |> 
  dplyr::arrange(Year) |> 
  dplyr::mutate(Year_lead = dplyr::lead(Year),
                Year_lead = ifelse(is.na(Year_lead), Year, Year_lead),
                Free_limit_lead = dplyr::lead(Free_limit),
                Free_limit_lead = ifelse(is.na(Free_limit_lead), Free_limit, Free_limit_lead)) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(ys = list(c(Free, Free_limit_lead)),
                xs = list(c(Year, Year_lead))) |> 
  dplyr::as_tibble()


dubois_equations <- purrr::map2_df(
  .x = dubois_lead$xs, 
  .y = dubois_lead$ys, 
  .f = function(.x, .y) {
    f <- lm(.y ~ .x)
    df <- data.frame(
      "intercept" = coefficients(f)[1],
      "m" = coefficients(f)[2], 
      row.names = NULL
    )
    return(df)
  }
)


df <- df |>
  dplyr::mutate(Year = round(year, -1),
                Year = ifelse(Year - year > 0, round(Year - 10, -1), Year))

dubois_final <- dubois_lead |> 
  dplyr::select(-ys, -xs) |> 
  dplyr::bind_cols(dubois_equations) |> 
  dplyr::right_join(df, by = "Year") |> 
  dplyr::mutate(new_value = ifelse(year < year_trick, m*year + intercept, value))


#TODO: adjust percent labels
# Plot ----------------------------------------------------------------------------------------------

library(ggplot2)

p <- ggplot(dubois_final) + 
  geom_area(data = df, aes(x = year, y = value), fill = "#141112", color = "#141112") +
  geom_area(aes(x = year, y = new_value), fill = "#c72545") +
  coord_flip() +
  scale_x_reverse(breaks = dubois$Year, labels = dubois$Year) + 
  scale_y_reverse(breaks = 0:3, labels = c("", "1%", "2%", "3%"), position = "right", expand = expansion(0, 0.5)) +
  geom_line(aes(x = year, y = new_value), color = "white") +
  geom_text(aes(x = Year, y = -0.25, label = Free_label), hjust = 0.5, size = 3) +
  geom_segment(data = dplyr::summarise(dplyr::group_by(dubois_final, Year), value = dplyr::first(value)),
               aes(y = 0, yend = value, x = Year, xend = Year), color = "white") +
  labs(
    title = "SLAVES AND FREE NEGROES",
    caption = "Source: Du Bois · Graphic: Matheus S. Rodrigues",
    x = "", y = ""
  ) +
  theme(
    plot.background = element_rect(fill = "#dfd0bf"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20, l = 120, r = 120, b = 20),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.ticks.y = element_blank()
  )

ggsave(filename = here::here("2022/week_07/plot.png"), plot = p, width = 7, height = 10, units = "in", dpi = 320)
