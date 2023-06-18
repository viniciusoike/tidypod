## code to prepare `DATASET` dataset goes here
library(sf)
library(here)

zones <- st_read(here("data-raw", "zones_od.gpkg"))
districts <- st_read(here("data-raw", "districts.gpkg"))
cities <- st_read(here("data-raw", "cities.gpkg"))

usethis::use_data(zones, districts, cities, overwrite = TRUE, compress = "xz")
