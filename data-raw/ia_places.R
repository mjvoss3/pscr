
library(dplyr)
library(tigris)
library(sf)



poly_place <- places(state = "IA", year = 2020)
poly_county <- counties(state = "IA", year = 2020)

poly_place <- poly_place |> mutate(area = st_area(poly_place))

# Geographical Relationships ----------------------------------------------

place_geos <- st_join(st_centroid(poly_place), poly_county |> transmute(geometry = geometry, county_name = NAMELSAD)) |>
  st_drop_geometry()

intersections <- st_intersection(poly_place, poly_county)
intersections <- intersections |> mutate(int_area = st_area(intersections), int_area_pct = as.numeric(int_area) / as.numeric(area))

maxes <- intersections |>
  st_drop_geometry() |>
  group_by(NAME) |>
  slice_max(order_by = int_area_pct, n = 1)

compare <- left_join(place_geos, maxes |> transmute(NAME = NAME, compare_county_name = NAMELSAD.1))


tract_geos <- st_join(poly_tract |> mutate(location_id = GEOID10), poly_place |> transmute(geometry = geometry, place_name = NAME)) |>
  st_drop_geometry() |>
  group_by(location_id) |>
  summarize(place_list = paste(place_name, collapse = ", ")) |>
  select(location_id, place_list)
zcta_geos <- st_join(poly_zcta |> mutate(location_id = ZCTA5CE10), poly_place |> transmute(geometry = geometry, place_name = NAME)) |>
  group_by(location_id) |>
  summarize(place_list = paste(place_name, collapse = ", ")) |>
  ungroup() |>
  st_join(poly_county |> transmute(geometry = geometry, county_name = NAMELSAD)) |>
  group_by(location_id, place_list) |>
  summarize(county_list = paste0(county_name, collapse = ", ")) |>
  st_drop_geometry()


ia_places <- left_join(poly_place, place_geos)
usethis::use_data(ia_places)
