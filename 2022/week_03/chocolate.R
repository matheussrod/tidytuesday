

# Sources -------------------------------------------------------------------------------------------

# http://flavorsofcacao.com/chocolate_database.html
#  Rating Scale
# 
#  - 4.0 - 5.0  = Outstanding
#  - 3.5 - 3.9  = Highly Recommended
#  - 3.0 - 3.49 = Recommended
#  - 2.0 - 2.9  = Disappointing
#  - 1.0 - 1.9  = Unpleasant


# Main questions ------------------------------------------------------------------------------------

# - What most memorable characteristics are present in the best ratings?


# Chocolate data ------------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 3)
chocolate <- tuesdata$chocolate

chocolate_characteristics <- chocolate |> 
  dplyr::select(ref, cocoa_percent, characteristics = most_memorable_characteristics, rating) |> 
  dplyr::mutate(cocoa_percent = as.double(stringr::str_remove(cocoa_percent, "%"))/100,
                characteristics = stringr::str_to_title(characteristics)) |> 
  tidyr::separate_rows(characteristics, sep = ", ")

chocolate_ratings <- dplyr::mutate(
    chocolate_characteristics, 
    rating_category = dplyr::case_when(
      rating < 2 ~ "Unpleasant",
      rating < 3 ~ "Disappointing",
      rating < 3.5 ~ "Recommended",
      rating < 4 ~ "Highly Recommended",
      TRUE ~ "Outstanding")) |> 
  dplyr::mutate(rating_category = factor(rating_category, levels = c("Unpleasant", "Disappointing", "Recommended", "Highly Recommended", "Outstanding")))

chocolate_top10 <- chocolate_ratings |> 
  dplyr::group_by(rating_category, characteristics) |> 
  dplyr::summarise(quantity = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(rating_category, dplyr::desc(quantity)) |> 
  dplyr::group_by(rating_category) |> 
  dplyr::mutate(row = dplyr::row_number()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(row <= 10)

chocolate_percent <- chocolate_top10 |> 
  dplyr::group_by(rating_category) |> 
  dplyr::mutate(percent = quantity / sum(quantity)) |> 
  dplyr::ungroup()


# Inspirations --------------------------------------------------------------------------------------

# [The 34 Best Interactive Data Visualizations from the New York Times - Dolphins](https://pin.it/2iyqmiU)


