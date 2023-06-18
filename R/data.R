#' POD Cities shape file
#'
#' A spatial `sf` object with the city borders of all POD cities.
#'
#' @format ## `cities`
#' A `sf` data.frame with 39 rows and 4 columns.
#' \describe{
#'   \item{code_muni}{Numeric code identifying each city.}
#'   \item{name_muni}{Name of the city.}
#'   \item{code_muni_ibge}{7-digit numeric code from IBGE.}
#'   \item{area_ha}{Area of the polygon in hectares (ha).}
#' }
#' @source <https://www.metro.sp.gov.br/pesquisa-od/>
"cities"

#' POD Districts and Regions
#'
#' A spatial `sf` object with the district borders all of POD cities.
#'
#' @format ## `districts`
#' A `sf` data.frame with 134 rows and 7 columns.
#' \describe{
#'   \item{code_district}{Numeric code identifying each district.}
#'   \item{name_district}{Name of the district.}
#'   \item{area_ha}{Area of the polygon in hectares (ha).}
#'   \item{is_cbd}{Binary variable (dummy) indicating if district is inside the São Paulo CBD.}
#'   \item{name_region}{Name of the region. Only applies for São Paulo}
#'   \item{region_simplified}{Simplified name of each region.}
#'   \item{code_region}{Numeric code identifying each region.}
#' }
#' @source <https://www.metro.sp.gov.br/pesquisa-od/>
"districts"

#' POD Zones
#'
#' A spatial `sf` data.frame with 517 rows and 11 columns.
#'
#' @format ## `zones`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{code_zone}{Numeric code identifying each OD zone.}
#'   \item{name_zone}{Name of the OD zone.}
#'   \item{code_muni}{Numeric code identifying each city.}
#'   \item{name_muni}{Name of the city.}
#'   \item{code_district}{Numeric code identifying each district.}
#'   \item{name_district}{Name of the district.}
#'   \item{area_ha}{Area of the polygon in hectares (ha).}
#'   \item{is_cbd}{Binary variable (dummy) indicating if district is inside the São Paulo CBD.}
#'   \item{name_region}{Name of the region. Only applies for São Paulo}
#'   \item{region_simplified}{Simplified name of each region.}
#'   \item{code_region}{Numeric code identifying each region.}
#' }
#' @source <https://www.metro.sp.gov.br/pesquisa-od/>
"zones"
