
library(dplyr)
library(tigris)
library(sf)



poly_place <- places(state = "IA", year = 2020)
poly_zcta <- zctas(state = "IA", year = 2010)
poly_county <- counties(state = "IA", year = 2020)


# Geographical Relationships ----------------------------------------------
zcta_geos <- st_join(poly_zcta, poly_place |> transmute(geometry = geometry, place_name = NAME)) |>
  group_by(ZCTA5CE10) |>
  summarize(place_list = paste(place_name, collapse = ", ")) |>
  ungroup() |>
  st_join(poly_county |> transmute(geometry = geometry, county_name = NAMELSAD)) |>
  group_by(ZCTA5CE10, place_list) |>
  summarize(county_list = paste0(county_name, collapse = ", ")) |>
  st_drop_geometry()


ia_zctas <- left_join(poly_zcta, zcta_geos)
usethis::use_data(ia_zctas)
