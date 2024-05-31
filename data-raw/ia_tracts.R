library(dplyr)
library(tigris)
library(sf)


# Pulling Data ------------------------------------------------------------

tracts <- tracts(state = "IA")
counties <- counties(state = "IA")
places <- places(state = "IA")
zctas <- zctas(state = "IA", year = 2010)


# Initial Cleaning --------------------------------------------------------

tracts <- tracts |>
  mutate(tracts_area = st_area(tracts))
places <- places |>
  transmute(place_name = NAME, place_geoid = GEOID, place_area = st_area(places))
zctas <- zctas |>
  transmute(zip_code = ZCTA5CE10, zcta_area = st_area(zctas))


# Places ------------------------------------------------------------------

place_int <- st_intersection(tracts, places)

place_int <- place_int |>
  mutate(int_area = st_area(place_int), prop = as.numeric(int_area) / as.numeric(tracts_area))

place_lists <- place_int |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarize(place_list = paste(place_name, collapse = ", "))
place_primary <- place_int |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  slice_max(order_by = prop, n = 1) |>
  ungroup() |>
  select(-c(place_area, int_area, prop))


# ZIPs --------------------------------------------------------------------

zcta_int <- st_intersection(tracts, zctas)

zcta_int <- zcta_int |>
  mutate(int_area = st_area(zcta_int |> sf::st_make_valid()), prop = as.numeric(int_area) / as.numeric(tracts_area))

zcta_lists <- zcta_int |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarize(zip_list = paste(zip_code, collapse = ", "))
zcta_primary <- zcta_int |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  slice_max(order_by = prop, n = 1) |>
  ungroup() |>
  select(-c(int_area, prop, zcta_area))


# Writing Data ------------------------------------------------------------

ia_tracts <- tracts |>
  left_join(place_lists) |>
  left_join(place_primary) |>
  left_join(zcta_lists) |>
  left_join(zcta_primary)

usethis::use_data(ia_tracts, overwrite = TRUE)
