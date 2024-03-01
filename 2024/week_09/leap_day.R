
# Analysis ------------------------------------------------------------------------------------

r <- 5/(2*pi)
df_lines <- data.frame(
  xmin = 1,
  xmax = 6,
  ymin = seq(0, -6, -2) * r,
  ymax = seq(0, -6, -2) * r,
  row_number = 1:4
) |> 
  dplyr::mutate(xmin = ifelse(row_number == max(row_number), 3.5, xmin))

df_arcs <- data.frame(
  x0 = c(6, 1, 6),
  y0 = seq(-1, -5, -2) * r,
  r = c(r, -r, r),
  start = 0,
  end = pi
)

arcs_x <- c(
  c(cos(r), r, cos(r)) + 6,
  1 - c(cos(r), r, cos(r)),
  c(cos(r), r, cos(r)) + 6
)
arcs_y <- rep(seq(0, -4, -2) * r, each = 3) - rep(r, 9) + c(sin(r), 0, -sin(r))

r2 <- r * 1.5
arcs_img_x <- c(
    c(cos(r), r2, cos(r)) + 6.2,
    0.8 - c(cos(r), r2, cos(r)),
    c(cos(r), r2, cos(r)) + 6.2
  )
arcs_img_y <- rep(seq(0, -4, -2) * r, each = 3) - rep(r, 9) + c(sin(r), 0, -sin(r)) * 1.3

df_arcs_points <- data.frame(
  position = c(7:9) + rep(0:2 * 9, each = 3),
  x = arcs_x,
  y = arcs_y,
  x_img = arcs_img_x,
  y_img = arcs_img_y,
  x_desc = arcs_x + c(c(0.65, 1.05, 0.65), c(0.65, 1.05, 0.65)*-1, c(0.65, 1.05, 0.65)),
  y_desc = arcs_img_y,
  hjust = rep(c(0, 1, 0), each = 3)
)

df_years <- dplyr::tibble(year = seq(1904, 2020, 4)) |> 
  dplyr::mutate(row_number = dplyr::row_number())

df_lines_points <- df_years |> 
  dplyr::filter(!(row_number %in% df_arcs_points$position)) |> 
  dplyr::mutate(
    x = c(1:6, 6:1, 1:6, 6:4),
    y = c(rep(seq(0, -4, -2) * r, each = 6), rep(-r * 6, 3))
  )

imgs_path <- here::here('2024/week_09/imgs')
df_imgs <- dplyr::tibble(img_path = list.files(imgs_path, full.names = TRUE)) |> 
  dplyr::mutate(year = as.integer(stringr::str_extract(img_path, '\\d{4}(?=\\.)'))) |> 
  dplyr::mutate(img_cropped = cropcircles::crop_square(img_path, border_size = 2))

df_years_desc <- dplyr::tribble(
  ~year, ~desc,
  1908, 'James Madison University is founded, Virginia in the United States as The State Normal',
  1912, 'The Piedra Movediza (Moving Stone) of Tandil falls and breaks',
  1916, 'Tokelau is annexed by the United Kingdom',
  1920, 'The Czechoslovak National Assembly adopts the Constitution',
  1936, 'The February 26 Incident in Tokyo ends',
  1940, 'Hattie McDaniel is the first African American to win an Academy Award.',
  1944, 'The Admiralty Islands are invaded in Operation Brewer, led by general Douglas MacArthur',
  1960, 'The 5.7 Mw Agadir earthquake shakes Morocco with intensity of X (Extreme)',
  1972, 'South Korea withdraws 11,000 of its 48,000 troops from Vietnam as part of Nixon\'s Vietnamization policy',
  1980, 'Gordie Howe makes NHL history as he scores his 800th goal',
  1984, 'Pierre Trudeau announces his retirement as Liberal Party leader',
  1988, 'Archbishop Desmond Tutu is arrested during a five-day anti-apartheid',
  1992, 'First day of Bosnia and Herzegovina independence referendum',
  1996, 'Faucett Perú Flight 251 crashes in the Andes; all 123 passengers and crew are killed',
  2000, 'Chechens attack a guard post near Ulus Kert',
  2004, 'Jean-Bertrand Aristide is removed as president of Haiti following a coup',
  2008, 'The United Kingdom\'s Ministry of Defence withdraws Prince Harry from a tour of Afghanistan',
  2012, 'North Korea suspend uranium enrichment and nuclear and long-range missile tests',
  2016, 'At least 40 people are killed and 58 others wounded following a suicide bombing by ISIL',
  2020, 'The United States and the Taliban sign the Doha Agreement for bringing peace to Afghanistan'
) |> 
  dplyr::mutate(desc = stringr::str_wrap(desc, width = 20))

df_fev29 <- df_years |> 
  dplyr::left_join(df_lines_points[,-1], by = 'row_number') |> 
  dplyr::left_join(df_arcs_points, by = c('row_number' = 'position'), suffix = c('', '_curve')) |> 
  dplyr::left_join(df_imgs, by = 'year') |> 
  dplyr::left_join(df_years_desc, by = 'year') |> 
  dplyr::mutate(
    x0 = ifelse(is.na(x), x_curve, x),
    y0 = ifelse(is.na(y), y_curve, y),
    x0_img = ifelse(is.na(x), x_img, x),
    y0_img = ifelse(is.na(y), y_img, y - 0.5*r),
    x_desc = ifelse(is.na(x), x_desc, x + 0.3),
    y_desc = ifelse(is.na(y), y_desc, y - 0.5*r - 0.7),
    hjust_desc = ifelse(is.na(x), hjust, 1)
  )

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')
sysfonts::font_add_google('Oswald', 'oswald')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
f1 <- 'outfit'
f2 <- 'noto'
f3 <- 'oswald'

p1 <- ggplot(df_fev29) + 
  geom_rect(
    data = df_lines,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    color = 'grey85', linewidth = 1, linetype = 1
  ) +
  ggforce::geom_arc(
    data = df_arcs,
    aes(x0 = x0, y0 = y0, r = r, start = start, end = end), 
    linewidth = 1, color = 'grey85', linetype = 1
  ) +
  geom_text(
    data = dplyr::filter(df_fev29, !is.na(img_path)),
    aes(x = x0_img, y = y0_img - 0.4, label = year), 
    hjust = 0, vjust = 0.5, family = f3
  ) +
  geom_text(
    aes(x = x_desc, y = y_desc - 0.285, label = desc, hjust = hjust_desc), 
    vjust = 1, size = 2.125, lineheight = 0.95, family = f2
  ) +
  ggimage::geom_image(
    aes(x = x0_img, y = y0_img, image = img_cropped),
    size = 0.085, hjust = 0.5
  ) +
  annotate(
    geom = 'text', 
    x = -1, y = -5.85, label = 'February 29', 
    family = f1, size = 13, hjust = 0, vjust = 0
  ) +
  annotate(
    geom = 'text', 
    x = -1, y = -5, label = paste0(
      'February 29 is a leap day, an intercalary date added periodically\n',
      'to create leap years in the Julian and Gregorian calendars.\n\n',
      
      'According to Wikipedia, it is possible to view notable events that\n',
      'occurred on that day in the 20th and 21st centuries.'
    ), 
    family = f2, size = 3, hjust = 0, vjust = 0
  ) +
  xlim(c(-1, 8.25)) +
  ylim(c(0.1, -6.25)) +
  labs(caption = 'Source: Wikipedia · Graphic: Matheus S. Rodrigues') +
  theme(
    plot.margin = margin(rep(15, 4)),
    plot.caption = element_text(family = f2, color = 'grey40'),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_09/plot.png'),
  plot = p1,
  width = 10,
  height = 8,
  dpi = 300
)
