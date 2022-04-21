

# Sources -------------------------------------------------------------------------------------------

# https://www.akc.org/
# https://www.usatoday.com/picture-gallery/life/2021/06/28/the-50-most-popular-dog-breeds-in-america/45134329/
# https://www.vox.com/2016/8/31/12715176/most-popular-dog-breeds


# Main questions ------------------------------------------------------------------------------------

# - What characteristics are best evaluated?


# Dog breeds data -----------------------------------------------------------------------------------

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_characteristic <-  breed_traits |> 
  dplyr::select(-`Coat Type`, -`Coat Length`) |> 
  tidyr::pivot_longer(cols = -1, names_to = "characteristic") |> 
  dplyr::group_by(characteristic) |> 
  dplyr::summarise(value = sum(value),
                   percent = value / (195*5)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(percent))


breed_details <- breed_characteristic |> 
  dplyr::left_join(trait_description[, 1:3], by = c("characteristic" = "Trait"))

# Inspirations --------------------------------------------------------------------------------------


