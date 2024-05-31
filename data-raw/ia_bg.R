library(dplyr)
library(tigris)
library(sf)


# Pulling Data ------------------------------------------------------------

bg <- block_groups(state = "IA")
counties <- counties(state = "IA")
places <- places(state = "IA")
zctas <- zctas(state = "IA", year = 2010)


# Initial Cleaning --------------------------------------------------------

bg <- bg |>
  mutate(bg_area = st_area(bg))
places <- places |>
  transmute(place_name = NAME, place_geoid = GEOID, place_area = st_area(places))
zctas <- zctas |>
  transmute(zip_code = ZCTA5CE10, zcta_area = st_area(zctas))


# County and Tract IDs ----------------------------------------------------

bg <- bg

ia_bg <- ia_bg |>
  mutate(
    county_geoid = paste0(STATEFP, COUNTYFP),
    tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
    .before = place_list
  )

# Places ------------------------------------------------------------------

place_int <- st_intersection(bg, places)

place_int <- place_int |>
  mutate(int_area = st_area(place_int), prop = as.numeric(int_area) / as.numeric(bg_area))

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

zcta_int <- st_intersection(bg, zctas)

zcta_int <- zcta_int |>
  mutate(int_area = st_area(zcta_int |> sf::st_make_valid()), prop = as.numeric(int_area) / as.numeric(bg_area))

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

ia_bg <- bg |>
  left_join(place_lists) |>
  left_join(place_primary) |>
  left_join(zcta_lists) |>
  left_join(zcta_primary)

usethis::use_data(ia_bg, overwrite = TRUE)
