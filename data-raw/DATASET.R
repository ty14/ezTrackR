## code to prepare `DATASET` dataset goes here

#usethis::use_data("DATASET")

## storing example datasets.

habit<-readr::read_csv("data-raw/SC_FullP_PostHab2_4_10_LocationOutput.csv")
cup<-readr::read_csv("data-raw/SC_FullP_SA5_4_10_LocationOutput.csv")

devtools::use_data(habit, overwrite=T)
devtools::use_data(cup, overwrite=T)
