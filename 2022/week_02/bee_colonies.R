

# Sources -------------------------------------------------------------------------------------------
 
# - https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-11
# - https://beeinformed.org/2021/06/21/united-states-honey-bee-colony-losses-2020-2021-preliminary-results/
# - https://ocm.auburn.edu/newsroom/news_articles/2021/06/241121-honey-bee-annual-loss-survey-results.php
# - https://www.theguardian.com/environment/2019/jun/19/us-beekeepers-lost-40-of-honeybee-colonies-over-past-year-survey-finds


# Main questions ------------------------------------------------------------------------------------

# - Are loss rates in the US similar to rates in other states?
# - Are stressors in other states the same as in the United States?


# Bee colonies data ---------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 2)
colony <- tuesdata$colony
stressor <- tuesdata$stressor


## Getting information only for other states and the United States ----------------------------------

states <- c("Other States", "United States")

columns_to_remove <- dplyr::all_of(c("colony_n", "colony_max", "colony_lost", "colony_added", "colony_reno"))

colony_states <- dplyr::filter(colony, state %in% states) |> 
  dplyr::select(-columns_to_remove)

# Getting only the main stressor for each year, month and state
stressor_states <- dplyr::filter(stressor, state %in% states) |> 
  dplyr::filter(!is.na(stress_pct)) |>
  dplyr::arrange(year, months, state, dplyr::desc(stress_pct)) |> 
  dplyr::group_by(year, months, state) |> 
  dplyr::summarise(stress_pct = max(stress_pct),
                   stressor = dplyr::first(stressor)) |> 
  dplyr::ungroup()

colony_stress <- colony_states |> 
  dplyr::inner_join(stressor_states, by = c("year", "months", "state"))


# Inspirations --------------------------------------------------------------------------------------

# 1.  [Bee With Ballonhand Finished Print of My Original Watercolour](https://pin.it/3AsyAnA)
# 2.  [Pattern Design Honey Bees & Bumble Bees — Amy Holliday Illustration](https://pin.it/5jm1VDg)


## Sketch -------------------------------------------------------------------------------------------

# img/bee_colonies.png


