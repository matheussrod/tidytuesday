
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 12)
mutant_moneyball  <- tuesdata$mutant_moneyball |> 
  janitor::clean_names()

# Analysis ------------------------------------------------------------------------------------
appearance <- mutant_moneyball |> 
  dplyr::select(member, dplyr::contains('appearance')) |> 
  dplyr::mutate(dplyr::across(
    dplyr::contains('appearance'),
    ~ as.double(stringr::str_remove(.x, '%')))
  ) |> 
  dplyr::rename_with(~ gsub('_appearance_percent', '', .x)) |> 
  tidyr::pivot_longer(cols = -member, names_to = 'period', values_to = 'percent') |> 
  dplyr::mutate(period_desc = dplyr::case_match(period,
    'x60s' ~ '1963 ~ 69',
    'x70s' ~ '1970 ~ 79',
    'x80s' ~ '1980 ~ 89',
    'x90s' ~ '1990 ~ 92'
  )) |> 
  dplyr::mutate(
    period_desc = ordered(period_desc, c('1963 ~ 69', '1970 ~ 79', '1980 ~ 89', '1990 ~ 92')),
    member = dplyr::case_match(member,
    'alexSummers' ~ 'Havok',
    'alisonBlaire' ~ 'Dazzler',
    'annaMarieLeBeau' ~ 'Rogue',
    'betsyBraddock' ~ 'Psylocke',
    'bobbyDrake' ~ 'Iceman',
    'charlesXavier' ~ 'Professor X',
    'ericMagnus' ~ 'Magneto',
    'hankMcCoy' ~ 'Beast',
    'jeanGrey' ~ 'Jean Grey',
    'johnProudstar' ~ 'Thunderbird',
    'jonathanSilvercloud' ~ 'Forge',
    'jubilationLee' ~ 'Jubilee',
    'kittyPryde' ~ 'Kitty Pryde',
    'kurtWagner' ~ 'Nightcrawler',
    'loganHowlett' ~ 'Wolwerine',
    'longshot' ~ 'Longshot',
    'lornaDane' ~ 'Polaris',
    'lucasBishop' ~ 'Bishop',
    'ororoMunroe' ~ 'Storm',
    'peterRasputin' ~ 'Colossus',
    'rachelSummers' ~ 'Askani',
    'remyLeBeau' ~ 'Gambit',
    'scottSummers' ~ 'Cyclops',
    'seanCassidy' ~ 'Banshee',
    'shiroYoshida' ~ 'Sunfire',
    'warrenWorthington' ~ 'Angel'
  ))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Bangers', 'bangers')
sysfonts::font_add_google('Nunito', 'nunito')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'bangers'
f2 <- 'nunito'

p1 <- ggplot(appearance) + 
  geom_tile(
    aes(
      x = period_desc,
      y = forcats::fct_reorder(member, percent),
      fill = percent,
    ),
    color = 'black',
  ) +
  scale_fill_gradient(
    low = '#f5d4cb', 
    high = '#8b3344',
    breaks = seq(0, 100, 25),
    labels = paste0(seq(0, 100, 25), '%'),
    guide = guide_colorbar(
      title = 'Appearance\npercentage',
      direction = 'horizontal',
      barwidth = 10,
      ticks = FALSE,
      nbin = 5,
    ),
  ) +
  labs(
    title = 'X-Men appearance',
    subtitle = paste0(
      'The percentage each X-Men member appeared in an issue\n',
      'published between 1963 and 1992.'
    ),
    caption = 'Source: Anderson Evans · Graphic: Matheus S. Rodrigues',
    x = NULL,
    y = NULL,
  ) +
  theme(
    plot.margin = margin(rep(30, 4)),
    plot.background = element_rect(
         fill = '#f7e9da',
    ),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    plot.title = element_text(
         family = f1,
         size = 30,
    ),
    plot.subtitle = element_text(
         family = f2,
         margin = margin(t = 5, b = 60),
    ),
    plot.caption = element_text(
         family = f2,
         hjust = 0,
         size = 8,
         color = 'grey40',
         margin = margin(t = 15),
    ),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(
         family = f2,
         size = 9,
    ),
    axis.text.x = element_text(
         size = 8,
    ),
    legend.position = c(0.18, 1.085),
    legend.background = element_blank(),
    legend.title = element_text(
           family = f2,
           size = 9,
           hjust = 1,
           vjust = 1,
    ),
    legend.text = element_text(
           family = f2,
           size = 8,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_12/plot.png'),
  plot = p1,
  width = 5,
  height = 8,
  dpi = 300,
)
