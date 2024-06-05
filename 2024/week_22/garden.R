
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 22)
harvest_2020 <- janitor::clean_names(tuesdata$harvest_2020)
harvest_2021 <- janitor::clean_names(tuesdata$harvest_2021)
planting_2020 <- janitor::clean_names(tuesdata$planting_2020)
planting_2021 <- janitor::clean_names(tuesdata$planting_2021)
spending_2020 <- janitor::clean_names(tuesdata$spending_2020)
spending_2021 <- janitor::clean_names(tuesdata$spending_2021)

# Functions -----------------------------------------------------------------------------------
rbind_with_year <- function(df1, df2) {
  df1$year = 2020
  df2$year = 2021
  df = rbind(df1, df2)
  return(df)
}

build_valid_date <- function(df) {
  df |> 
    dplyr::filter(!is.na(date)) |> 
    dplyr::mutate(
      date = ifelse(year == 2020, date, date - lubridate::years(1)),
      date = as.Date(date)
    ) |> 
    dplyr::mutate(year = factor(year))
}

get_min_max <- function(df) {
  df |> 
    dplyr::group_by(year) |> 
    dplyr::reframe(
      date_min = min(date),
      date_max = max(date)
    ) |> 
    dplyr::mutate(dplyr::across(dplyr::starts_with('date'), as.integer))
}

convert_range <- function(x, type='date') {
  min_date_int <- as.integer(as.Date('2020-04-01'))
  max_date_int <- as.integer(as.Date('2020-12-01'))
  range_date <- c(min_date_int, max_date_int)
  range_int <- c(0, 10)
  if (type == 'date') {
    range <- scales::rescale(x, to = range_int, from = range_date)
  } else {
    range <- scales::rescale(x, to = range_date, from = range_int)
  }
  return(range)
}

# Analysis ------------------------------------------------------------------------------------
planting <- rbind_with_year(planting_2020, planting_2021) |> 
  build_valid_date() |> 
  dplyr::select(date, seeds = number_seeds_planted, year)

harvest <- rbind_with_year(harvest_2020, harvest_2021) |> 
  build_valid_date() |> 
  dplyr::select(date, weight, year)

planting_min_max <- dplyr::mutate(get_min_max(planting), action = 'planting')
harvest_min_max <- dplyr::mutate(get_min_max(harvest), action = 'harvest')
  
rects <- rbind(planting_min_max, harvest_min_max) |> 
  dplyr::mutate(dplyr::across(dplyr::starts_with('date'),  convert_range)) |> 
  dplyr::mutate(
    xmin = ifelse(year == '2021', 1, -1),
    xmax = ifelse(year == '2021', 2, -2),
    ymin = date_min,
    ymax = date_max,
    fill = rep(c('#401e12', '#5a7302'), each = 2)
  )

rects_label <- rects |> 
  tidyr::pivot_longer(cols = c(date_min, date_max)) |> 
  dplyr::transmute(
    x_label = ifelse(year == '2021', xmax + 1, xmax - 1),
    y_label = value,
    date = as.Date(convert_range(value, 'int')),
    month_day = paste0(
      stringr::str_pad(lubridate::month(date), 2, 'left', '0'), 
      '/', lubridate::day(date)
    ),
    label = dplyr::case_when(
      action == 'planting' & name == 'date_min' ~ paste0('Start of planting\n', month_day),
      action == 'planting' & name == 'date_max' ~ paste0('End of planting\n', month_day),
      action == 'harvest' & name == 'date_min' ~ paste0('Start of harvest\n', month_day),
      action == 'harvest' & name == 'date_max' ~ paste0('End of harvest\n', month_day)
    ),
    hjust = ifelse(year == '2021', 0, 1),
    vjust = 0.5,
    
    # geom_segment
    xstart = ifelse(year == '2021', xmax + 0.25, xmax - 0.25),
    xend = ifelse(year == '2021', xmax + 0.75, xmax - 0.75),
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Merriweather', 'merriweather')
sysfonts::font_add_google('Poppins', 'poppins')

f1 <- 'poppins'
f2 <- 'merriweather'

p1 <- ggplot() + 
  ## rects ------------------------------------------------------------------------------------
  geom_rect(
    data = rects,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin, 
      ymax = ymax,
      fill = fill,
    ),
    alpha = 0.5,
    show.legend = FALSE,
  ) +
  
  ## middle line ------------------------------------------------------------------------------
  geom_segment(
    aes(
      x = 0,
      xend = 0,
      y = 0, 
      yend = 10,
    ),
    color = 'grey85',
  ) +
  
  ## years ------------------------------------------------------------------------------------
  annotate(
    'text',
    x = c(-1.5, 1.5),
    y = 0.5,
    label = c('2020', '2021'),
    family = f2,
    size = 3,
    hjust = 0.5,
  ) +
  
  ## texts ------------------------------------------------------------------------------------
  geom_text(
    data = rects_label,
    aes(
      x = x_label,
      y = y_label,
      label = label,
      hjust = hjust,
      vjust = vjust,
    ),
    family = f2,
    lineheight = 1,
    size = 2.75,
  ) +
  geom_segment(
    data = rects_label,
    aes(
      x = xstart,
      xend = xend,
      y = y_label,
      yend = y_label,
    ),
    linetype = 3,
    color = 'grey60',
  ) +
  
  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'Lisa\'s garden',
    subtitle = 'How did planting and harvesting change\nfrom 2020 to 2021?',
    caption = 'Source: {gardenR} · Graphic: Matheus S. Rodrigues',
  ) +
  
  ## scales & coords --------------------------------------------------------------------------
  scale_y_reverse() +
  scale_fill_identity() +
  coord_fixed(clip = 'off') +
  
  ## theme ------------------------------------------------------------------------------------
  theme_void() +
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(
         fill = 'white',
         color = NA,
    ),
    plot.title = element_text(
         family = f1,
         size = 20,
         color = '#5a7302',
         face = 'bold',
         hjust = 0.5,
    ),
    plot.subtitle = element_text(
         family = f2,
         size = 10,
         color = '#5a7302',
         hjust = 0.5,
    ),
    plot.caption = element_text(
         size = 8,
         family = f2,
         hjust = 0.5,
         color = 'grey40',
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_22/plot.png'),
  plot = p1,
  width = 6,
  height = 7,
  dpi = 300,
)
