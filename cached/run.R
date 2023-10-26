library(tidypod)
library(here)
library(vroom)

tbls_pod = import_pod_tables(cached = FALSE, tables = "all")
tbls_travel = import_pod_travel_tables(cached = FALSE, tables = "all")
geo_pod = import_pod_tables(cached = FALSE, geo = TRUE, tables = "all")
geo_travel = import_pod_travel_tables(cached = FALSE, geo = TRUE, tables = "all")

# Export to compressed csv using vroom
vroom_write(tbls_pod, here("cached", "tbl_pod.csv.gz"))
# Export to compressed RDS using readr
readr::write_rds(tbls_travel, here("cached", "tbl_pod_travel.rds"), compress = "gz")
readr::write_rds(geo_pod, here("cached", "geo_pod.rds"), compress = "gz")
readr::write_rds(geo_travel, here("cached", "geo_pod_travel.rds"), compress = "gz")
