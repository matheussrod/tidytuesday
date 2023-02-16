
library(dplyr)

# Hollywood Age Gaps ------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 7)
age_gaps <- tuesdata$age_gaps

age_longer <- age_gaps |>
  select(year = release_year, ends_with('gender'), ends_with('age')) |>
  tidyr::pivot_longer(
    cols = starts_with('character'),
    names_to = 'character',
    values_to = 'gender'
  ) |>
  mutate(
    age = ifelse(stringr::str_detect(character, '1'), actor_1_age, actor_2_age)
  ) |> 
  select(year, gender, age)

age_points <- age_longer |>
  arrange(year, age, gender) |>
  mutate(
    age_factor = case_when(
      age <= 25 ~ '< 26',
      age <= 40 ~ '26 ~ 40',
      age <= 55 ~ '41 ~ 55',
      age <= 70 ~ '56 ~ 70',
      age <= 85 ~ '71 ~ 85'
    )
  ) |>
  mutate(age_factor = forcats::fct_reorder(age_factor, age, .desc = TRUE)) |>
  group_by(year, gender) |>
  mutate(row = row_number()) |>
  ungroup() |>
  mutate(row = ifelse(gender == 'woman', -row - 1, row + 1)) |>
  dplyr::select(-age)

# Plot --------------------------------------------------------------------
library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Outfit', 'outfit')
f1 <- 'outfit'

label_gender <- tibble(
  x = c(1935, 1935), y = c(8, -8), label = c('man', 'woman')
)

years <- tibble(
  x = c(seq(1935, 2015, 10), 2022), y = -55,
  label = c(seq(1935, 2015, 10), 2022)
)

pal <- c('#A7DDF2', '#13678A', '#45C4B0', '#9AEBA3', '#DAFDBA')
background <- '#262626'

p1 <- ggplot(age_points) +
  geom_text(
    data = label_gender, aes(x = x, y = y, label = label),
    size = 5, hjust = 0, family = f1, color = 'white'
  ) +
  geom_segment(aes(x = 1934, xend = 2023, y = 0, yend = 0), color = 'white') +
  geom_point(aes(x = year, y = row, color = age_factor), size = 2) +
  geom_text(
    data = years, aes(x = x, y = y, label = label),
    family = f1, color = 'white', size = 3.5, hjust = 0.5
  ) +
  scale_color_manual(
    values = c(
      '< 26' = pal[[1]],
      '26 ~ 40' = pal[[2]],
      '41 ~ 55' = pal[[3]],
      '56 ~ 70' = pal[[4]],
      '71 ~ 85' = pal[[5]]
    ),
    guide = guide_legend(
      title = 'Age', direction = 'horizontal', reverse = TRUE,
      override.aes = list(size = 5)
    )
  ) +
  annotate(
    geom = 'text', x = 1933.5, y = 45, label = 'Love is in the air age',
    size = 13, hjust = 0, color = 'white', family = f1
  ) +
  geom_segment(
    aes(x = 1963, xend = 1969.5, y = 44.5, yend = 44.5),
    color = 'white', linewidth = 1.5
  ) +
  annotate(
    geom = 'text', x = 1933.5, y = 37,
    label = paste0(
      'Assessment of the ages of men and women at the time of the release\n',
      'of Hollywood movie love interests.\n',
      'Each dot represents a man or a woman.'
    ),
    size = 5, hjust = 0, color = 'white', family = f1, lineheight = 0.75
  ) +
  annotate(
    geom = 'text', x = 1933.5, y = -60,
    label = 'Source: Hollywood Age Gap · Graphic: Matheus S. Rodrigues',
    size = 3.5, hjust = 0, color = 'grey90', family = f1
  ) +
  theme_void() +
  theme(
    text = element_text(family = f1),
    panel.background = element_rect(fill = background),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.78),
    legend.title = element_text(color = 'white', size = 12),
    legend.text = element_text(family = f1, color = 'white', size = 12),
    legend.spacing.x = unit(0.05, 'cm'),
    legend.key = element_blank()
  )

showtext_opts(dpi = 300)
ggsave(
  filename = here::here("2023/week_07/plot.png"),
  plot = p1,
  width = 10,
  height = 10,
  dpi = 300
)
