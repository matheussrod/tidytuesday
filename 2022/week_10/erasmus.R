
# Erasmus data --------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 10)
erasmus <- janitor::clean_names(tuesdata$erasmus)

erasmus_total <- erasmus |>
  dplyr::rename(sending_country = sending_country_code,
                receiving_country = receiving_country_code) |> 
  dplyr::mutate(mobility_start_date = as.Date(paste0(mobility_start_month, '-01'))) |> 
  dplyr::count(mobility_start_date, sending_country, receiving_country, name = 'qty') |> 
  tidyr::pivot_longer(cols = c(sending_country, receiving_country),
                      names_to = 'category', values_to = 'country') |> 
  dplyr::mutate(qty = ifelse(category == 'sending_country', -qty, qty)) |> 
  dplyr::group_by(mobility_start_date, country) |> 
  dplyr::summarise(total = sum(qty))

top10_countries <- erasmus_total |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(total = sum(abs(total))) |> 
  dplyr::arrange(dplyr::desc(total)) |> 
  dplyr::slice_head(n = 10) |> 
  dplyr::pull(country)

erasmus_top10 <- erasmus_total |> 
  dplyr::ungroup() |> 
  dplyr::filter(country %in% top10_countries) |> 
  dplyr::mutate(is_positive = as.factor(ifelse(total >= 0, 'Increased', 'Decreased'))) |> 
  dplyr::mutate(country = forcats::fct_reorder(country, dplyr::desc(total)))


# Plot --------------------------------------------------------------------

library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Roboto', 'roboto')

 p1 <- ggplot(erasmus_top10) + 
  geom_col(aes(x = mobility_start_date, y = total, fill = is_positive)) +
  scale_fill_manual(name = 'Balance', values = c("#A64646", "#032859")) +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") +
  facet_wrap(~ country, ncol = 1, strip.position = 'left') +
  labs(
    title = 'Countries that most received or sent students through the Erasmus program',
    subtitle = 'The top 10 countries were obtained considering the entire period and considering only the month in which the mobility began.
The results are a balance of incoming students minus outgoing students.',
    caption = 'Source: Data.Europa · Graphic: Matheus S. Rodrigues',
    x = '', y = ''
  ) +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = 'roboto', face = 'bold', size = 15),
    plot.subtitle = element_text(family = 'roboto', color = 'grey40', size = 10, 
                                 margin = margin(b = 15)),
    plot.caption = element_text(family = 'roboto', color = 'grey40', hjust = 0),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'roboto', size = 7),
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(family = 'roboto', angle = 0),
    strip.background = element_blank(),
    legend.title = element_text(family = 'roboto', size = 9),
    legend.text = element_text(family = 'roboto', size = 8),
    legend.position = 'top',
    legend.key.height = unit(.35, units = 'cm'),
    legend.key.width = unit(.35, units = 'cm')
  )

showtext_opts(dpi = 500)
ggsave(
  filename = here::here("2022/week_10/plot.png"), 
  plot = p1, 
  width = 9, 
  height = 6,
  dpi = 500
)
