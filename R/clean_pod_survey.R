#' @importFrom rlang .data
clean_srv <- function(df) {

  df <- as.data.frame(df)

  df <- df |>
    dplyr::rename(code_zone = zona) |>
    tidyr::pivot_longer(
      cols = -code_zone,
      names_to = "variable",
      values_to = "value"
    )

  df <- srv_compute_estimates(df)

  # df <- df |>
  #   dplyr::mutate(
  #     # Remove special characters
  #     variable = stringi::stri_trans_general(.data$variable, id = "ascii"),
  #     # Separate point estimates from confidence interval estimates
  #     estimate = dplyr::if_else(
  #       stringr::str_detect(.data$variable, "^se"),
  #       "confidence_interval",
  #       "point_estimate"),
  #     criteria = stringr::str_remove(.data$variable, "se\\.")
  #   ) |>
  #   tidyr::pivot_wider(
  #     id_cols = c("code_zone", "criteria"),
  #     names_from = "estimate",
  #     values_from = "value"
  #     )

  df <- df |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(
      group_percentage = .data$point_estimate / sum(.data$point_estimate) * 100) |>
    dplyr::ungroup()

  return(df)

}

#' @importFrom rlang .data
clean_srv_muni <- function(df) {

  # Convert to data.frame
  df <- as.data.frame(df)

  df <- df |>
    dplyr::rename(dplyr::all_of(c("name_muni" = "muni_dom"))) |>
    tidyr::pivot_longer(
      cols = -name_muni,
      names_to = "variable",
      values_to = "value")

  df <- srv_compute_estimates(df)

  return(df)

}

#' @importFrom rlang .data
srv_compute_estimates <- function(df) {

  df <- df |>
    dplyr::mutate(
      # Remove special characters
      variable = stringi::stri_trans_general(.data$variable, id = "ascii"),
      # Separate point estimates from confidence interval estimates
      estimate = dplyr::if_else(
        stringr::str_detect(.data$variable, "^se"),
        "confidence_interval",
        "point_estimate"),
      criteria = stringr::str_remove(.data$variable, "se\\.")) |>
    tidyr::pivot_wider(
      id_cols = c("code_zone", "criteria"),
      names_from = "estimate",
      values_from = "value") |>
    dplyr::mutate(
      estimate_upper = .data$point_estimate + .data$confidence_interval,
      estimate_lower = .data$point_estimate - .data$confidence_interval)

  return(df)

}
