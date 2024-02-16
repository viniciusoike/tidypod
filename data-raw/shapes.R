# Cleans shapefile geometries
library(sf)
library(dplyr)
library(readr)
sf::sf_use_s2(FALSE)

# Function to clean geometries and convert to 4326 CRS
clean_geometry <- function(shp) {
  # Remove Z-axis
  shp <- st_zm(shp)
  # Convert to standard Mercator 4326 projection
  shp <- st_transform(shp, crs = 4326)
  # Make sure geometries are valid
  shp <- st_make_valid(shp)
}

# Import raw shapefiles
muni <- st_read(here::here("data-raw/OD-2017/Mapas-OD2017/Shape-OD2017/Municipios_2017_region.shp"))
dstr <- st_read(here::here("data-raw/OD-2017/Mapas-OD2017/Shape-OD2017/Distritos_2017_region.shp"))
zone <- st_read(here::here("data-raw/OD-2017/Mapas-OD2017/Shape-OD2017/Zonas_2017_region.shp"))

# Cities

# Clean geometry
muni <- clean_geometry(muni)
# Rename columns
muni <- muni |>
  rename(
    code_muni = NumeroMuni,
    name_muni = NomeMunici,
    code_muni_ibge = CD_IBGE,
    area_ha = Area_ha
  )

# District

# Clean geometry
dstr <- clean_geometry(dstr)
# Rename columns
dstr <- dstr |>
  rename(
    code_district = NumeroDist,
    name_district = NomeDistri,
    area_ha = Area_ha
  )

# Match districts with cities
dstr_cities <- dstr |>
  st_centroid() |>
  st_join(select(muni, code_muni, name_muni)) |>
  st_drop_geometry()

dstr <- dstr |>
  select(code_district) |>
  left_join(dstr_cities, by = "code_district")

# Zone

# Clean geometry
zone <- clean_geometry(zone)
# Rename columns
zone <- zone |>
  rename(
    code_zone = NumeroZona,
    name_zone = NomeZona,
    code_muni = NumeroMuni,
    name_muni = NomeMunici,
    code_district = NumDistrit,
    name_district = NomeDistri,
    area_ha = Area_ha_2
  )

# Join POD zones and District with Regions and CBD

# Import the shape file for the CBD Zone
centro_expandido <- st_read("data-raw/shapesMiniAnel/shapesMiniAnel.shp")
# Create a dummy variable to identify zones inside the CBD
centro_expandido <- centro_expandido |>
  select(-everything()) |>
  mutate(is_cbd = 1) |>
  st_make_valid()

# Shrink the CBD shape file by a bit to increase precision of the join
shrink_cbd <- centro_expandido |>
  st_transform(crs = 32722) |>
  st_buffer(dist = -500) |>
  st_transform(crs = 4326) |>
  st_make_valid()
# Join zones with CBD
# Note: there isn't a 1:1 overlap. For more precise mapping of information
# a spatial/areal interpolation is needed
zone <- st_join(zone, shrink_cbd)
zone <- st_make_valid(zone)
zone <- mutate(zone, is_cbd = ifelse(is.na(is_cbd), 0L, 1L))

dstr <- st_join(dstr, shrink_cbd)
dstr <- st_make_valid(dstr)
dstr <- mutate(dstr, is_cbd = ifelse(is.na(is_cbd), 0L, 1L))

# Import table with districts and regions (only for SP)
district_regions <- read_csv("data-raw/district_regions.csv")

# Auxiliar data.frame with code_regions for each region
id_regions <- tribble(
  ~code_region, ~name_region,
  1, "Centro",
  2, "Leste I",
  3, "Centro-Sul",
  4, "Centro-Oeste",
  5, "Norte II",
  6, "Norte I",
  7, "Leste II",
  8, "Sul",
  9, "Oeste"
)

# Join regions with codes and simplify the name of each region
district_regions <- district_regions |>
  mutate(
    region_simplified = stringr::str_replace(name_region, "( )|(-)", "_"),
    region_simplified = stringr::str_to_lower(region_simplified)
  ) |>
  left_join(id_regions, by = "name_region")

# Join zones and districts with regions
zone <- left_join(zone, district_regions, by = "name_district")
dstr <- left_join(dstr, district_regions, by = "name_district")

# Export
st_write(zone, "data-raw/zones_od.gpkg", append = FALSE)
st_write(dstr, "data-raw/districts.gpkg", append = FALSE)
st_write(muni, "data-raw/cities.gpkg", append = FALSE)
