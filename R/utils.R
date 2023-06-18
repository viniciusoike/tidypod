
utils::globalVariables(
  c("ipca_adjust",
    "ipc_fipe_adjust",
    "correcao_pib",
    "correcao_pnad_nominal",
    "correcao_pnad_real",
    "cities",
    "districts",
    "zones"))

add_od_dimension <- function(dat) {


  id_zones <- tibble::as_tibble(sf::st_drop_geometry(zones))
  dat <- dplyr::left_join(dat, id_zones, by = "code_zone")
  dat <- dplyr::select(dplyr::all_of(names(id_zones)), dplyr::everything())

  return(dat)

}

# De abril-2018 até jan-2021
ipca_adjust <- 1.14618520
ipc_fipe_adjust <- 1.1609

# PIB (2018/1 - 2021/1) Fonte: SEADE
correcao_pib <- (105.4 / 96)
# PNAD (2018/1 - 2020/1) Fonte: PNAD
correcao_pnad_nominal <- (4165 / 3790) ^ (3 / 2)
correcao_pnad_real <- (4181 / 4127) ^ (3 / 2)


