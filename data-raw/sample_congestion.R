## code to prepare `sample_congestion` dataset goes here
library(xtradata)
library(data.table)
library(purrr)
library(lubridate)

key <- Sys.getenv("XTRADATA_KEY")
typename <- "CI_TRAFI_L"
rangeStart <- "2021-11-01"
rangeEnd <- "2021-11-02"

histo_congestion <- xtradata_requete_aggregate(key = key, 
                                               typename = typename, 
                                               rangeStart = rangeStart, 
                                               rangeEnd = rangeEnd, 
                                               rangeStep = "hour",
                                               attributes = list("etat" = "accumulateex", "ident" = "first"),
                                               showURL = TRUE)
setDT(histo_congestion)
# on va calculer un score de congestion à partir de ca
scores_referentiels <- data.table(value = c("FLUIDE", "DENSE", "EMBOUTEILLE", "INCONNU"), score = c(1,2,3,NA))

scores_horaires_ponderes <- map(histo_congestion$etat, function(.x) {
  
  dt_x <- as.data.table(.x)
  score <- merge(dt_x, scores_referentiels, by = "value") %>% 
    .[,score_pondere := ratio * score]
  sum(score$score_pondere)
})

histo_congestion %>% 
  .[, scores_horaires_ponderes := unlist(scores_horaires_ponderes)]

sample_congestion <- histo_congestion %>% 
  .[, etat := NULL]

usethis::use_data(sample_congestion, overwrite = TRUE)



### sur une plus grande periode de temps
library(tictoc)
nweeks <- 122
list_weeks <- as.list(0:(nweeks-1))
scores_referentiels <- data.table(value = c("FLUIDE", "DENSE", "EMBOUTEILLE", "INCONNU"), score = c(1,2,3,NA))

histo_long_congestion <- function(nweeks_to_add, startDate, scores_referentiels) {
  key <- Sys.getenv("XTRADATA_KEY")
  typename <- "CI_TRAFI_L"
  
  rangeStart <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add))
  rangeEnd <- lubridate::add_with_rollback(as_date(startDate), weeks(nweeks_to_add + 1))

  histo_congestion <- xtradata_requete_aggregate(key = key, 
                                                 typename = typename, 
                                                 rangeStart = rangeStart, 
                                                 rangeEnd = rangeEnd, 
                                                 rangeStep = "hour",
                                                 attributes = list("etat" = "accumulateex"),
                                                 showURL = TRUE) %>% 
    as.data.table()

    scores_horaires_ponderes <- map(histo_congestion$etat, function(.x) {
    
    dt_x <- as.data.table(.x)
    
    correctif_ratio <- 1
 
    score <- merge(dt_x, scores_referentiels, by = "value")
    
    if(any(score$value != "INCONNU")) {
      correctif_ratio <- max(score$ratio[score$value != "INCONNU"])
    }
    
    score <- score %>% 
      .[, ratio := ratio / correctif_ratio] %>% 
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

