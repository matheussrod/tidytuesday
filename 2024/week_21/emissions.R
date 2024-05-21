
# Data ----------------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 21)
emissions <- janitor::clean_names(tuesdata$emissions)

# Analysis ------------------------------------------------------------------------------------
## categories ---------------------------------------------------------------------------------
emissions_categories <- emissions |> 
  dplyr::mutate(
    commodity = dplyr::case_when(
      stringr::str_detect(commodity, 'Oil') ~ 'Oil',
      stringr::str_detect(commodity, 'Coal') ~ 'Coal',
      .default = commodity
    )
  )

## per decade ---------------------------------------------------------------------------------
emissions_per_decade <- emissions_categories |> 
  dplyr::mutate(decade = year - year %% 10) |> 
  dplyr::group_by(decade, commodity) |> 
  dplyr::summarise(
    unit = dplyr::last(production_unit),
    production_value = sum(production_value),
  ) |> 
  dplyr::ungroup() |>
  dplyr::mutate(first_decade = min(decade), .by = 'commodity')

## wider --------------------------------------------------------------------------------------
commodity <- emissions_per_decade |> 
  tidyr::pivot_wider(
    names_from = decade,
    values_from = production_value,
    names_prefix = 'decade_'
  ) |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with('decade'),
      ~ifelse(is.na(.x), 0, .x)
    )
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(total = sum(decade_1860:decade_2020)) |> 
  dplyr::as_tibble()

## yoy ----------------------------------------------------------------------------------------
emissions_yoy <- emissions_per_decade |> 
  dplyr::mutate(
    production_lag = dplyr::lag(production_value),
    yoy_production = (production_value - production_lag)/production_lag
  ) |> 
  dplyr::group_by(commodity) |> 
  dplyr::reframe(yoy_production = mean(yoy_production, na.rm = TRUE)) |> 
  dplyr::mutate(yoy_production_label = scales::label_percent()(yoy_production))

## labels -------------------------------------------------------------------------------------
label_number <- scales::label_number(accuracy = 0.01, scale_cut = scales::cut_short_scale())
commodity_labels <- commodity |> 
  dplyr::left_join(emissions_yoy, by = 'commodity') |> 
  dplyr::mutate(
    total_label = label_number(total),
    total_label = paste0(
      '**', total_label, '**',
      '<br>',
      ifelse(
        yoy_production < 0, 
        paste0("<span style=color:#fe6d07;>", '▼</span> ', yoy_production_label),
        paste0("<span style=color:#5873a3;>", '▲</span> ', yoy_production_label)
      ),
      " <span style=color:#7f7f7f;>avg. yoy</span>",
      '<br>',
      "<span style=color:#7f7f7f;>*since ", first_decade, '*</span>'
    )
  ) |>
  dplyr::arrange(dplyr::desc(yoy_production))

# Plot ----------------------------------------------------------------------------------------
library(gt)

g1 <- commodity_labels |> 
  gt() |> 
  ## cols hide --------------------------------------------------------------------------------
  cols_hide(
    columns = c(
      first_decade, 
      yoy_production, 
      yoy_production_label,
      total
    )
  ) |> 
  ## header and source ------------------------------------------------------------------------
  tab_header(
    title = md('Carbon Majors Fossil Fuel and Cement Emissions'),
    subtitle = 'Total production and progression since 1850',
  ) |> 
  tab_source_note(
    source_note = 'Source: Carbon Majors · Table: Matheus S. Rodrigues'
  ) |> 
  ## nanoplot ---------------------------------------------------------------------------------
  cols_nanoplot(
    columns = starts_with('decade'),
    plot_type = 'line',
    new_col_name = 'nanoplots',
    new_col_label = 'Progression',
    options = nanoplot_options(
      show_data_points = FALSE,
      show_y_axis_guide = TRUE,
      data_area_fill_color = '#fe6d07',
      data_line_stroke_color = '#72361b'
    ),
  ) |> 
  ## formating --------------------------------------------------------------------------------
  fmt_markdown(
    columns = total_label,
  ) |> 
  fmt_number(
    columns = total,
    suffixing = TRUE
  ) |> 
  ## col labels -------------------------------------------------------------------------------
  cols_label(
    commodity = 'Commodity',
    total_label = 'Production',
    unit = 'Unit'
  ) |>
  ## style ------------------------------------------------------------------------------------
  cols_width(
    commodity ~ px(110),
    unit ~ px(90),
    total_label ~ px(135)
  ) |> 
  tab_style(
    style = cell_text(
      font = google_font('Outfit'),
      weight = 'bold'
    ),
    locations = cells_title('title')
  ) |> 
  tab_style(
    style = cell_text(
      font = google_font('Noto Sans'),
      weight = 'normal'
    ),
    locations = list(
      cells_title('subtitle'),
      cells_source_notes()
    )
  ) |> 
  tab_style(
    style = cell_text(
      font = google_font('Lato'),
      weight = 'normal',
      size = px(12)
    ),
    locations = list(
      cells_column_labels(),
      cells_body()
    )
  ) |> 
  tab_style(
    style = cell_text(
      font = google_font('Noto Sans'),
      weight = 'normal',
      color = 'grey65',
      size = px(10)
    ),
    locations = cells_source_notes()
  )

gtsave(
  g1,
  filename = here::here('2024/week_21/plot.png')
)
