


#' Get ACS data for multiple Iowa geographies in one dataframe
#'
#' This function returns a dataframe with specified variables (or variables modified by a function) for all specified goegraphy levels
#' @param geos A character vector of geography levels. Can be any of block group, tract, place, or county
#' @param variables A character vector of variable IDs
#' @param output Determines the format of the output dataframe; one of wide or long/tidy
#' @param .fn Function for data modification. This function will be applied to the data at every geography level and requires preservation of the GEOID variable with the same name
#' @param ... Other arguments that will be passed to tidycensus::get_acs()
#' @return A dataframe with specified ACS data
#' @export
#' @examples
#' # ACS variables to be passed to tidycensus::get_acs()
#' variables <- c("B03002_001", "B03002_003") # Total population & non-Hispanic, white alone population
#' # Creating a modifying function for interpretable calculations
#' data_function <- function(data){data |> dplyr::transmute(GEOID = GEOID, pop_total = B03002_001E, white_pct = B03002_003E / B03002_001E)}
#' # Loading data in wide form
#' wide_data <- get_multi_geo_acs(variables = c("B03002_001", "B03002_003"), state = "IA", .fn = data_function)
#' head(wide_data)
#' # Loading data in long form
#' long_data <- get_multi_geo_acs(variables = c("B03002_001", "B03002_003", "B01001_001"), state = "IA", output = "tidy", .fn = data_function)
#' head(long_data)
get_multi_geo_acs <- function(geos = c("block group", "tract", "place", "county"), variables, output = "wide", .fn = NULL, ...){

  column_mods <- c(
    "block group" = "bg",
    "tract" = "tract",
    "place" = "place",
    "county" = "county"
  )

  data_list <- lapply(
    geos,
    FUN = function(x){
      tidycensus::get_acs(geography = x, variables = variables, output = "wide", state = "IA", keep_geo_vars = FALSE, geometry = FALSE)
    }
  )

  names(data_list) <- geos


  if(!is.null(.fn)){
    data_list <- lapply(
      data_list,
      FUN = .fn
    )

    # Check for GEOID columns
    name_check <- lapply(data_list, FUN = function(x){"GEOID" %in% names(x)}) |> simplify2array()
    if(FALSE %in% name_check){
      stop(".fn does not preserve GEOID column")
    }
  }

  data_list <- lapply(
    geos,
    FUN = function(x){
      data_list[[x]] |>
        dplyr::rename_with(.fn = function(y){paste0(column_mods[[x]], "_", y)})
    }
  )


  names(data_list) <- geos

  data <- combine_multi_acs(data_list)

  if(output %in% c("tidy", "long")){
    data <- data |>
      tidyr::pivot_longer(
        cols = -tidyselect::contains(c("geoid", "GEOID", "NAME", "name", "list")),
        names_pattern = "(bg_|tract_|place_|county_)(.*)",
        names_to = c(".value", "variable")
      ) |>
      dplyr::rename_with(
        .cols = tidyselect::any_of(c("bg_", "tract_", "place_", "county_")),
        function(x){paste0(x, "value")}
      )
  }

  return(data)

}


# data_function <- function(data){
#   data |>
#     dplyr::transmute(
#       GEOID = GEOID,
#       white_pct = B03002_003E / B03002_001E #
#     )
# }
# test_data <- get_multi_geo_acs(variables = c("B03002_001", "B03002_003"), state = "IA", .fn = data_function)
#
# test_long <- get_multi_geo_acs(variables = c("B03002_001", "B03002_003", "B01001_001"), state = "IA", output = "tidy")


combine_multi_acs <- function(named_list){

  if(!is.null(named_list[["block group"]])){

    data(ia_bg)

    return_data <- dplyr::left_join(
      named_list[["block group"]],
      ia_bg |> sf::st_drop_geometry() |> dplyr::select(GEOID, county_geoid, tract_geoid, place_list, place_name, place_geoid),
      by = c("bg_GEOID" = "GEOID")
    )

    if(!is.null(named_list[["tract"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["tract"]],
        by = c("tract_geoid" = "tract_GEOID")
      )
    }

    if(!is.null(named_list[["place"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["place"]],
        by = c("place_geoid" = "place_GEOID")
      )
    }

    if(!is.null(named_list[["county"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["county"]],
        by = c("county_geoid" = "county_GEOID")
      )
    }

    return(return_data)
  }

  if(!is.null(named_list[["tract"]])){

    data(ia_tracts)

    return_data <- dplyr::left_join(
      named_list[["tract"]],
      ia_tracts |> sf::st_drop_geometry() |> dplyr::select(GEOID, county_geoid, place_list, place_name, place_geoid),
      by = c("tract_GEOID" = "GEOID")
    )

    if(!is.null(named_list[["place"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["place"]],
        by = c("place_geoid" = "place_GEOID")
      )
    }

    if(!is.null(named_list[["county"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["county"]],
        by = c("county_geoid" = "county_GEOID")
      )
    }

    return(return_data)
  }

  if(!is.null(named_list[["place"]])){

    data(ia_places)

    return_data <- dplyr::left_join(
      named_list[["place"]],
      ia_places |> sf::st_drop_geometry() |> dplyr::select(GEOID, county_geoid),
      by = c("place_GEOID" = "GEOID")
    )

    if(!is.null(named_list[["county"]])){
      return_data <- dplyr::left_join(
        return_data,
        named_list[["county"]],
        by = c("county_geoid" = "county_GEOID")
      )
    }

    return(return_data)
  }

  if(!is.null(named_list[["county"]])){
    return(named_list[["county"]])
  }

}
