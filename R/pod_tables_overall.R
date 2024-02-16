new_names <- c(
  "code_zone" = "COD_ZONA",
  "name_zone" = "NOME",
  "hh" = "DOMICILIOS",
  "fami" = "FAMILIAS",
  "pop" = "POPULACAO",
  "enroll" = "MATRICULAS",
  "jobs" = "EMPREGOS",
  "cars" = "AUTOMOVEIS",
  "travels_origin" = "VIAGENS_PRODUZIDAS",
  "travles_destination" = "VIAGENS_ATRAIDAS",
  "area_ha" = "AREA_HA",
  "age_group" = "FAIXA_ETARIA",
  "educ_attainment" = "GRAU_INSTRUCAO",
  "income_group" = "FAIXA_RENDA",
  "income" = "TIPO_RENDA",
  "sex" = "GENERO",
  "cars" = "NUMERO_AUTOMOVEIS",
  "job_bond" = "VINCULO_EMPREGATICIO",
  "value" = "QTD"
)

pod_table_1 <- function(con) {

  dplyr::tbl(con, tables[3]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

}

pod_table_2 <- function(con, wide = FALSE) {

  tbl_age <- dplyr::tbl(con, tables[15]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  if (wide) {
    tbl_age <- tidyr::pivot_wider(
      tbl_age,
      id_cols = "code_zone",
      names_from = "age_group",
      values_from = "value"
    )
  }

  return(tbl_age)

}

pod_table_3 <- function(con, wide = FALSE) {

  tbl_educ <- dplyr::tbl(con, tables[17]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  if (wide) {
    tbl_educ <- tidyr::pivot_wider(
      tbl_educ,
      id_cols = "code_zone",
      names_from = "educ_attainment",
      values_from = "value"
    )
  }

  return(tbl_educ)

}

pod_table_4 <- function(con, wide = FALSE) {

  tbl_sex <- dplyr::tbl(con, tables[16]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  if (wide) {
    tbl_sex <- tidyr::pivot_wider(
      tbl_sex,
      id_cols = "code_zone",
      names_from = "sex",
      values_from = "value"
    )

    names(tbl_sex)[c(2, 3)] <- c("male", "female")

  }

  return(tbl_sex)

}

pod_table_5 <- function(con, wide = FALSE) {

  tbl_inc <- dplyr::tbl(con, tables[14]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  tbl_inc <- tbl_inc |>
    dplyr::mutate(income_group = stringr::str_replace_all(
      .data[["income_group"]],
      c("até" = "<", " a " = "-", "mais de" = ">"))
      )

  if (wide) {
    tbl_inc <- tidyr::pivot_wider(
      tbl_inc,
      id_cols = "code_zone",
      names_from = "income_group",
      values_from = "value"
    )
  }

  return(tbl_inc)

}

pod_table_6 <- function(con, wide = FALSE) {

  tbl_inc <- dplyr::tbl(con, tables[19]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  if (wide) {
    tbl_inc <- tidyr::pivot_wider(
      tbl_inc,
      id_cols = "code_zone",
      names_from = "income",
      values_from = "value"
    )

    names(tbl_inc)[2:4] <- c(
      "average_family_income", "per_capita_income", "median_family_income"
      )
  }

  return(tbl_inc)

}

pod_table_7 <- function(con, wide = FALSE, name_repair = TRUE) {

  tbl_car <- dplyr::tbl(con, tables[11]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  tbl_car <- tbl_car |>
    dplyr::mutate(
      cars = dplyr::case_when(
        .data[["cars"]] == "Nenhum Automóvel" ~ "No car",
        .data[["cars"]] == "1 Automóvel" ~ "1 car",
        .data[["cars"]] == "2 Automóveis" ~ "2 cars",
        .data[["cars"]] == "3 Automóveis ou Mais" ~ "3 cars or more",
        TRUE ~ "Total Families"
      )
    )

  if (wide) {

    tbl_car <- tidyr::pivot_wider(
      tbl_car,
      id_cols = "code_zone",
      names_from = "cars",
      values_from = "value"
    )

    if (name_repair) { tbl_car <- janitor::clean_names(tbl_car) }

  }

  return(tbl_car)

}

pod_table_8 <- function(con, wide = FALSE, name_repair = TRUE) {

  tbl_job <- dplyr::tbl(con, tables[18]) |>
    dplyr::rename(dplyr::any_of(new_names)) |>
    dplyr::collect()

  tbl_job |>
    dplyr::mutate(
      dplyr::case_when(
        "Assalariado...2" ~ "formal"
      )
    )


}

# get_pod_table("7", con)
# get_pod_table("7", con, wide = TRUE)









# dplyr::tbl(con, tables[8])
