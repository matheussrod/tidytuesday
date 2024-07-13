
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 28)
drob_funs <- janitor::clean_names(tuesdata$drob_funs)

# Analysis ------------------------------------------------------------------------------------
tidyverse_pkgs <- c(
  'ggplot',
  'dplyr',
  'tidyr',
  'readr',
  'purrr',
  'tibble',
  'stringr',
  'forcats'
)

pkg_fun_count <- drob_funs |> 
  dplyr::count(pkgs, funs, name = 'qty') |> 
  dplyr::arrange(dplyr::desc(qty)) |> 
  dplyr::mutate(
    is_tidy_pkg = pkgs %in% tidyverse_pkgs,
    fill = ifelse(is_tidy_pkg, '#1a162d', 'grey65')
  )

tidy_pct <- pkg_fun_count |> 
  dplyr::summarise(qty = sum(qty), .by = is_tidy_pkg) |> 
  dplyr::mutate(pct = scales::percent_format()(qty / sum(qty)))

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
library(treemapify)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 125)
sysfonts::font_add_google('Outfit', 'outfit')
sysfonts::font_add_google('Noto Sans', 'noto')

f1 <- 'outfit'
f2 <- 'noto'

p1 <- ggplot(
  data = pkg_fun_count,
  aes(
    area = qty,
    label = funs,
    group = funs,
    subgroup = forcats::fct_reorder(pkgs, is_tidy_pkg),
    fill = fill,
  )
) +
  
  ## treemap ----------------------------------------------------------------------------------
  geom_treemap() +
  geom_treemap_subgroup_border(
    color = 'white',
  ) +
  
  ## treemap text -----------------------------------------------------------------------------
  geom_treemap_subgroup_text(
    place = 'centre',
    grow = TRUE,
    alpha = 0.25,
    color = 'white',
    family = f1,
  ) +
  geom_treemap_text(
    color = 'white',
    family = f2,
  ) +
  
  ## scales -----------------------------------------------------------------------------------
  scale_fill_identity() +

  ## labs -------------------------------------------------------------------------------------
  labs(
    title = 'David Robinson\'s TidyTuesday Functions',
    subtitle = paste0(
      tidy_pct$pct[1],
      ' of the functions used by David are from ',
      "<b><span style='color:#1a162d;'>tidyverse packages</span></b>"
    ),
    caption = 'Source: David Robinson · Graphic: Matheus S. Rodrigues',
  ) +
    
  ## theme ------------------------------------------------------------------------------------
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.title = element_text(
         family = f1,
         size = 23,
    ),
    plot.subtitle = ggtext::element_markdown(
         family = f2,
         size = 11,
    ),
    plot.caption = element_text(
         family = f2,
         color = 'grey40',
         size = 8,
         hjust = 0,
    ),
  )

showtext::showtext_opts(dpi = 300)
ggsave(
  filename = here::here('2024/week_28/plot.png'),
  plot = p1,
  width = 10,
  height = 7,
  dpi = 300,
)
