

# Sources -------------------------------------------------------------------------------------------

# https://commemorativeairforce.org/
# https://en.wikipedia.org/wiki/Tuskegee_Airmen


# Main questions ------------------------------------------------------------------------------------


# Tuskegee Airmen data ------------------------------------------------------------------------------

library(stringr)

tuesdata <- tidytuesdayR::tt_load(2022, week = 6)
airmen <- tuesdata$airmen

#Sys.getlocale("LC_TIME")
airmen_jets <- airmen |> 
  dplyr::filter(!is.na(aerial_victory_credits)) |> 
  dplyr::select(name, victory_cred = aerial_victory_credits) |> 
  tidyr::separate_rows(victory_cred, sep = ";") |> 
  tidyr::separate_rows(victory_cred, sep = "and") |> 
  dplyr::mutate(victory_cred = str_squish(victory_cred),
                jet = ifelse(str_detect(victory_cred, "on"), str_extract(victory_cred, "(?<=\\d\\s).*(?=[\\s]on)"), str_extract(victory_cred, "(?<=\\d\\s).*$")),
                jet_n = ifelse(str_detect(victory_cred, "Downed"), str_extract(victory_cred, "(?<=Downed\\s)[1-3\\/\\s]*(?=[\\s])"), str_extract(victory_cred, "[1-3\\/]*(?=[\\s])")),
                victory_dt = as.Date(str_extract(victory_cred, "(?<=on) .*$"), format = " %B %d, %Y")) |> 
  tidyr::fill(victory_dt, .direction = "up")

airmen_jets$jet <- stringr::str_remove(airmen_jets$jet, "1/2 ")
airmen_jets$jet_n <- as.integer(with(airmen_jets, ifelse(jet_n == "1/2", "2", ifelse(jet_n == "1 1/2", 2, jet_n))))

airmen_jets_n <- airmen_jets |> 
  dplyr::mutate(group_dt = paste0(lubridate::year(victory_dt), "-", lubridate::month(victory_dt), "-", "01")) |> 
  dplyr::group_by(group_dt) |> 
  dplyr::summarise(jet_n = sum(jet_n)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(group_dt = as.Date(group_dt, format = "%Y-%m-%d")) |> 
  dplyr::arrange(group_dt, jet_n)

airmen_names <- airmen_jets |> 
  dplyr::select(name) |> 
  dplyr::distinct() |> 
  dplyr::mutate(x = rep(1:8, 9), y = rep(1:8, each = 9))

airmen_xy <- airmen_names |> 
  dplyr::filter(x == y) |> 
  dplyr::group_by(x, y) |> 
  dplyr::mutate(y = y + c(0.3, -0.3))


# Inspirations --------------------------------------------------------------------------------------


