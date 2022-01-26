## code to prepare `sample_congestion` dataset goes here
library(xtradata)
library(data.table)
library(purrr)
library(lubridate)

key <- Sys.getenv("XTRADATA_KEY")
typename <- "CI_TRAFI_L"

### sur une plus grande periode de temps
library(tictoc)
nweeks <- 122
list_weeks <- as.list(0:(nweeks-1))
scores_referentiels <- data.table(value = c("FLUIDE", "DENSE", "EMBOUTEILLE", "INCONNU"), score = c(1,2,3,NA))

histo_long_congestion <- function(nweeks_to_add, startDate, scores_referentiels, filter = NULL) {
  key <- Sys.getenv("XTRADATA_KEY")
  typename <- "CI_TRAFI_L"
  
  rangeStart <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add))
  rangeEnd <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add + 1))

  histo_congestion <- xtradata_requete_aggregate(key = key, 
                                                 typename = typename, 
                                                 rangeStart = rangeStart, 
                                                 rangeEnd = rangeEnd, 
                                                 rangeStep = "hour",
                                                 filter = filter,
                                                 attributes = list("etat" = "accumulateex"),
                                                 showURL = TRUE) %>% 
    as.data.table()

    scores_horaires_ponderes <- map(histo_congestion$etat, function(.x) {
      
    dt_x <- as.data.table(.x)
    
    correctif_ratio <- 1
 
    score <- merge(dt_x, scores_referentiels, by = "value")
    
    if(any(score$value == "INCONNU") & nrow(score)>1) {
      # correctif_ratio <- max(score$ratio[score$value != "INCONNU"])
      correctif_ratio <- 1/sum(score$ratio[score$value != "INCONNU"])
    }
    
    score <- score %>% 
      .[, ratio := ratio * correctif_ratio] %>% 
      .[,score_pondere := ratio * score]
    sum(score$score_pondere, na.rm = TRUE)
  })
    
  histo_congestion %>% 
    .[, scores_horaires_ponderes := unlist(scores_horaires_ponderes)] %>% 
    .[, etat := NULL]
  
}
histo_long_congestion_possibly <- possibly(histo_long_congestion, otherwise = NULL)

tic()
congestion_78weeks  <- map(list_weeks, ~histo_long_congestion_possibly(nweeks_to_add = .x, startDate = "2019-09-01", scores_referentiels = scores_referentiels))
toc()

# usethis::use_data(congestion_78weeks , overwrite = TRUE)

congestion_from_sept2019_dt <- rbindlist(congestion_78weeks) %>% unique()

usethis::use_data(congestion_from_sept2019_dt, overwrite = TRUE)

# congestion_from_sept2019_dt[scores_horaires_ponderes>3, lubridate::as_date(time)] %>% unique() %>% sort()
# on a tjs des pb a posteriori, avant l'historique pré 04/2020 (intégré par AW)
# on remplacera les valeurs problématiques par des 3



congestion_cleaned <- copy(congestion::congestion_from_sept2019_dt) %>% 
  .[scores_horaires_ponderes >3, scores_horaires_ponderes := 3] %>% 
  .[scores_horaires_ponderes == 0, scores_horaires_ponderes := NA] %>% 
  .[scores_horaires_ponderes <1, scores_horaires_ponderes := 1] %>% 
  .[, time := lubridate::as_datetime(time, tz = "Europe/Paris")] %>% 
  .[, date := lubridate::as_date(time)] %>% 
  .[, hour := data.table::hour(time)] %>% 
  .[, day := data.table::wday(time)] %>% 
  .[, HPM := hour %in% 7:9] %>% 
  .[, HPS := hour %in% 17:19] %>% 
  .[, JO := day %in% 2:6]


usethis::use_data(congestion_cleaned, overwrite = TRUE)





# recherche de bug
# congestion_from_sept2019_dt[gid == 1305 & as_date(as_datetime(time, tz = "Europe/Paris")) == as_date("2021-12-20")]
GID 1305
2021-12-20 à 8h
library(xtradata)
library(data.table)
library(purrr)
library(lubridate)

a <- map(as.list(0:1), ~histo_long_congestion_possibly(nweeks_to_add = .x, startDate = "2021-12-20", scores_referentiels = scores_referentiels, filter = list("gid" = 1305)))
