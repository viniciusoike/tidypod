get_pod_microdata <- function(dir, id, labelled = TRUE) {

  stopifnot(id %in% c("pessoa", "domicilio", "familia", "viagem"))

  if (missing(dir)) {
    dir <- here::here("data/sp_pod/od2017_transparencia/Banco de Dados-OD2017")
    pod <- haven::read_sav(here::here(dir, "OD_2017_v1.sav"))
  } else {
    pod <- haven::read_sav(dir)
  }

  if (labelled) {

    pod <- pod |>
      dplyr::mutate(
        dplyr::across(
          -dplyr::contains(c("fe_", "renda_", "qt_", "idade", "zona", "co_", "id_")),
          labelled_ascii)
      )

  }

  # Income bands (obs: 954 is the minimum wage at the time)
  faixas <- c(4, 6, 10, 15, 20, 25, 30) * 954

  pod <- pod |>
    mutate(
      renda_fa_ipca = renda_fa * (ipca_correcao + correcao_pnad_real - 1),
      renda_fa_ipc_fipe = renda_fa * (ipc_fipe_correcao + correcao_pnad_real - 1),
      d_estuda = ifelse(estuda == "Nao", "NaoEstuda", "Estuda"),
      faixas_renda = findInterval(renda_fa, faixas, rightmost.closed = T),
      d_banh = ifelse(qt_banho > 0, 1, qt_banho)
    )


  pod <- design_pod(df = pod, id = id)

  return(pod)

}

labelled_ascii <- function(x) {
  x <- haven::as_factor(x)
  x <- forcats::fct_relabel(x, stringi::stri_trans_general, id = "Latin-ASCII")
  return(x)
}

create_mpod_tables <- function(tables = "all") {

  hh <- get_pod_microdata(id = "hh")

}

mpod_table_living <- function(dat) {

  condmora <- survey::svyby(~condmora, ~zona, dat, survey::svytotal)
  condmora <- clean_srv(condmora)

  return(condmora)

}

# condmora <- svyby(~condmora, ~zona, domi, svytotal)
# condmora <- clean_srv(condmora)

# create_mpod_tables()







