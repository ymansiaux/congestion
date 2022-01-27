## code to prepare `sample_congestion` dataset goes here
library(xtradata)
library(data.table)
library(purrr)
library(lubridate)

library(tictoc)
nweeks <- 52
list_weeks <- as.list(0:(nweeks-1))


histo_temps_parcours <- function(nweeks_to_add, startDate) {
  
  key <- Sys.getenv("XTRADATA_KEY")
  typename <- "CI_TPSTJ_A"
  
  rangeStart <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add))
  rangeEnd <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add + 1))
  
  xtradata_requete_aggregate(
    key = key,
    typename = typename,
    rangeStart = rangeStart,
    rangeEnd = rangeEnd,
    rangeStep = "hour",
    attributes = list("actuel"="average", "ref"="average"),
    showURL = TRUE
  ) %>% 
    as.data.table()
  

}
histo_temps_parcours_possibly <- possibly(histo_temps_parcours, otherwise = NULL)

tic()
temps_parcours  <- map(list_weeks, ~histo_temps_parcours_possibly(nweeks_to_add = .x, startDate = "2021-01-11"))
toc()

temps_parcours_dt <- rbindlist(temps_parcours, fill = TRUE) %>% unique()

usethis::use_data(temps_parcours_dt, overwrite = FALSE)
