

# Sources -------------------------------------------------------------------------------------------

# https://boardgamegeek.com/
# https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews


# Main questions ------------------------------------------------------------------------------------

# Does the maximum and minimum play time decrease over time?
# Is play time related to the minimum age?


# Board games data ----------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

details <- tuesdata$details
ratings <- tuesdata$ratings

details_time <- details |> 
  dplyr::group_by(year = yearpublished) |> 
  dplyr::summarise(min_play = mean(minplaytime),
                   quantity = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(year >= 2001, year <= 2022)


# Inspirations --------------------------------------------------------------------------------------
