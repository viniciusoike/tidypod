#' Create the survey design
#'
#' Create the a survey design for the POD microdata
#'
#' @param df A `data.frame` with POD microdata.
#' @param id Choice of identifier. Must be one of `person` (person),
#' `family` (family), or `hh` (household).
#'
#' @return A survey object
#' @export
design_pod <- function(df, id) {

  stopifnot(is.data.frame(df))

  if(id == "pess") {
    out <- survey::svydesign(
      ids = ~ 1,
      weights = ~ fe_pess,
      strata = ~ zona,
      data = dplyr::distinct(df, .data$id_pess, .keep_all = TRUE)
    )
  }
  if(id == "fami") {
    out <- survey::svydesign(
      ids = ~ 1,
      weights = ~ fe_fam,
      strata = ~ zona,
      data = dplyr::distinct(df, .data$id_fam, .keep_all = TRUE)
    )
  }
  if(id == "domi") {
    out <- survey::svydesign(
      ids = ~ 1,
      weights = ~ fe_dom,
      strata = ~ zona,
      data = dplyr::distinct(df, .data$id_dom, .keep_all = TRUE)
    )
  }
  return(out)
}
