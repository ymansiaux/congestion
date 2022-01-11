usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("deploy_to_RSPM.R")
usethis::use_r("_disable_autoload")
usethis::use_build_ignore("R/_disable_autoload.R")

usethis::use_vignette("analyse_congestion")


usethis::use_data_raw("sample_congestion")



usethis::use_vignette("occupationparkingsapp")
usethis::use_vignette("questions")
usethis::use_vignette("designApp")




vignette <- FALSE
devtools::check(document = TRUE, vignettes = vignette)
devtools::build(vignettes = vignette)
devtools::install(build_vignettes = vignette)

usethis::use_test("occupation_compute_xtradata_request_parameters")


pkgload::load_all()

golem::sanity_check()

golem::run_dev()
