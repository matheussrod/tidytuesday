
# Cats UK -----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 05)
cats_uk_reference <- tuesdata$cats_uk_reference

reference <-  dplyr::select(cats_uk_reference, prey_p_month, age_years)

reference <- dplyr::mutate(reference, prey_p_month = as.character(prey_p_month))
reference <- rbind(
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '0' = '0', other = c('0.5', '3', '7.5', '12.5', '17.5'))),
    highlight = "0"
  ), 
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '0.5' = "0.5", other = c('0', '3', '7.5', '12.5', '17.5'))),
    highlight = "0.5"
  ),
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '3' = "3", other = c('0', '0.5', '7.5', '12.5', '17.5'))),
    highlight = "3"
  ),
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '7.5' = "7.5", other = c('0', '0.5', '3', '12.5', '17.5'))),
    highlight = "7.5"
  ), 
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '12.5' = "12.5", other = c('0', '0.5', '3', '7.5', '17.5'))),
    highlight = "12.5"
  ),
  dplyr::mutate(reference,
    prey_p_month = as.character(forcats::fct_collapse(prey_p_month, '17.5' = "17.5", other = c('0', '0.5', '3', '7.5', '12.5'))),
    highlight = "17.5"
  )
) |> 
  dplyr::mutate(
    highlight = factor(highlight, levels = c('0', '0.5', '3', '7.5', '12.5', '17.5', 'other'))
  )


# Plot --------------------------------------------------------------------
library(ggplot2)
library(ggridges)
library(showtext)

showtext_auto()
font_add_google('Noto sans', 'noto')
font_add_google('Noto sans display', 'noto_display')

f1 <- 'noto'
f2 <- 'noto_display'

facet_wrap_label <- c(
  '0' = '0 preys',
  '0.5' = '1 prey',
  '3' = '3 preys',
  '7.5' = '8 preys',
  '12.5' = '13 preys',
  '17.5' = '18 preys'
)

p1 <- ggplot(reference, aes(x = age_years)) + 
  annotate(geom = 'rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = 'transparent', color = 'grey95') +
  geom_density_line(aes(y = after_stat(count), fill = forcats::fct_rev(prey_p_month)), 
                    color = 'transparent', position = 'fill', alpha = 0.85, 
                    show.legend = FALSE) +
  facet_wrap(~ highlight, nrow = 1, labeller = as_labeller(facet_wrap_label)) +
  scale_y_continuous(name = 'Relative proportion', expand = c(0, 0), 
                     labels = scales::percent) +
  scale_x_continuous(name = 'Cat years') +
  scale_fill_manual(
    values = c("transparent", rep('#492312', 6))
  ) +
  labs(
    title = 'The hunt of the cats',
    subtitle = paste0(
      'Distribution of preys captured by cats analyzed by "Movebank for Animal Tracking Data" by age.\n', 
      'The amount of preys captured is, approximately, the amount of preys captured per month.'
    ),
    caption = 'Source: Movebank for Animal Tracking Data · Graphic: Matheus S. Rodrigues'
  ) +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(rep(30, 4)),
    plot.title = element_text(family = f1, face = 'bold', size = 20, hjust = 0.5),
    plot.subtitle = element_text(family = f1, size = 8.5, hjust = 0.5, 
                                 margin = margin(b = 20)),
    plot.caption = element_text(family = f1, hjust = 0.5, color = 'grey40', 
                                size = 7, margin = margin(t = 10)),
    strip.background = element_blank(),
    strip.text = element_text(f1, size = 8, color = 'grey40'),
    axis.text = element_text(family = f2, color = 'grey40', size = 8),
    axis.title = element_text(family = f2, color = 'grey40', size = 8),
    axis.ticks = element_line(color = 'grey80')
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_05/plot.png"), 
  plot = p1, 
  width = 8, 
  height = 5,
  dpi = 300
)



