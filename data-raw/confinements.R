confinements <- data.frame(
  deb = c(as.Date("2020-03-17"), as.Date("2020-10-30"), as.Date("2021-04-03")),
  fin = c(as.Date("2020-05-10"), as.Date("2020-12-14"), as.Date("2021-05-03")),
  label = c("Confinement", "Confinement", "Confinement")
)

usethis::use_data(confinements, overwrite = TRUE)
