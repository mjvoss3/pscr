#' Iowa Place Data
#'
#' A data frame of Iowa places. Data from tigris, with a column added for county, determined by which county the place centroid is located in.
#' @source U.S. Census Bureau TIGER/LINE
"ia_places"

#' Iowa Census Tract Data
#'
#' A data frame of Iowa places. Data from tigris, with a column added for a list of places that intersect with each Census tract.
#' @source U.S. Census Bureau TIGER/LINE
"ia_tracts"

#' Iowa ZCTA Data
#'
#' A data frame of Iowa zip code tabulation areas. Data from tigris, with columns added for place and county lists. The lists are determined by places and counties in which the ZCTA intersects.
#' @source U.S. Census Bureau TIGER/LINE
"ia_zctas"

#' Iowa Census Block Group Data
#'
#' A data frame of Iowa block groups. Data from tigris, with columns added for county, tract, place, and zip code information. Corresponding primary places and ZIP codes are determined by the place or ZIP code that takes up the largest amount of the block group's area.
#' @source U.S. Census Bureau TIGER/LINE
"ia_bg"
