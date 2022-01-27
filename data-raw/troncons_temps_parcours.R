library(sf)
temps_parcours_geo <- read_sf("/data/donnees_geo/CI_TRAFI_L/temps_trajet.json")

usethis::use_data(temps_parcours_geo, overwrite = FALSE)
