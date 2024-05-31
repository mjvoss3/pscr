
#' View an sf data frame
#'
#' This function removes the geometry from an sf data frame for purposes of viewing the data in RStudio.
#' This can be useful because sf geometry can significantly slow down RStudio's View() function
#'
#' @param sf_data An object with class 'sf' and class 'data.frame'
#' @export
st_view <- function(sf_data){
  View(sf_data |> sf::st_drop_geometry())
}

#' Clean an Address Vector
#'
#' This function cleans addresses by making them more readable, primarily correcting case.
#' @param addresses A character vector of addresses
#' @return A character vector with title case capitalization, with corrected single letter and direction capitalization
#' @export
clean_addresses = function(addresses){
  # Create title case and fix "IA" and "US-" unique cases
  addresses <- addresses |> tolower()
  addresses <- addresses |> tools::toTitleCase()
  addresses <- addresses |> stringr::str_replace("#", "Unit")
  addresses <- addresses |> stringr::str_replace_all("Ia", "IA")
  addresses <- addresses |> stringr::str_replace_all("Us-", "US-")

  # Fix directions on stringr::street names in middle of stringr::string
  addresses <- addresses |> stringr::str_replace_all(" n ", " N ")
  addresses <- addresses |> stringr::str_replace_all(" s ", " S ")
  addresses <- addresses |> stringr::str_replace_all(" e ", " E ")
  addresses <- addresses |> stringr::str_replace_all(" w ", " W ")
  addresses <- addresses |> stringr::str_replace_all(" Ne ", " NE ")
  addresses <- addresses |> stringr::str_replace_all(" Nw ", " NW ")
  addresses <- addresses |> stringr::str_replace_all(" Se ", " SE ")
  addresses <- addresses |> stringr::str_replace_all(" Sw ", " SW ")

  # Fix some letters for stringr::street names
  addresses <- addresses |> stringr::str_replace_all(" a ", " A ")
  addresses <- addresses |> stringr::str_replace_all(" b ", " B ")
  addresses <- addresses |> stringr::str_replace_all(" c ", " C ")
  addresses <- addresses |> stringr::str_replace_all(" d ", " D ")
  addresses <- addresses |> stringr::str_replace_all(" e ", " E ")
  addresses <- addresses |> stringr::str_replace_all(" f ", " F ")
  addresses <- addresses |> stringr::str_replace_all(" g ", " G ")
  addresses <- addresses |> stringr::str_replace_all(" h ", " H ")
  addresses <- addresses |> stringr::str_replace_all(" i ", " I ")
  addresses <- addresses |> stringr::str_replace_all(" j ", " J ")
  addresses <- addresses |> stringr::str_replace_all(" k ", " K ")
  addresses <- addresses |> stringr::str_replace_all(" l ", " L ")
  addresses <- addresses |> stringr::str_replace_all(" m ", " M ")
  addresses <- addresses |> stringr::str_replace_all(" n ", " N ")


  # Fix directions at beginning of stringr::string
  addresses <- addresses |> stringr::str_replace_all("^n ", "N ")
  addresses <- addresses |> stringr::str_replace_all("^s ", "S ")
  addresses <- addresses |> stringr::str_replace_all("^e ", "E ")
  addresses <- addresses |> stringr::str_replace_all("^w ", "W ")
  addresses <- addresses |> stringr::str_replace_all("^Ne ", "NE ")
  addresses <- addresses |> stringr::str_replace_all("^Nw ", "NW ")
  addresses <- addresses |> stringr::str_replace_all("^Se ", "SE ")
  addresses <- addresses |> stringr::str_replace_all("^Sw ", "SW ")

  # Fix directions at end of stringr::string
  addresses <- addresses |> stringr::str_replace_all(" n$", " N")
  addresses <- addresses |> stringr::str_replace_all(" s$", " S")
  addresses <- addresses |> stringr::str_replace_all(" e$", " E")
  addresses <- addresses |> stringr::str_replace_all(" w$", " W")
  addresses <- addresses |> stringr::str_replace_all(" Ne$", " NE")
  addresses <- addresses |> stringr::str_replace_all(" Nw$", " NW")
  addresses <- addresses |> stringr::str_replace_all(" Se$", " SE")
  addresses <- addresses |> stringr::str_replace_all(" Sw$", " SW")

  # Fix directions right before comma
  addresses <- addresses |> stringr::str_replace_all(" n,", " N,")
  addresses <- addresses |> stringr::str_replace_all(" s,", " S,")
  addresses <- addresses |> stringr::str_replace_all(" e,", " E,")
  addresses <- addresses |> stringr::str_replace_all(" w,", " W,")
  addresses <- addresses |> stringr::str_replace_all(" Ne,", " NE,")
  addresses <- addresses |> stringr::str_replace_all(" Nw,", " NW,")
  addresses <- addresses |> stringr::str_replace_all(" Se,", " SE,")
  addresses <- addresses |> stringr::str_replace_all(" Sw,", " SW,")

  return(addresses)
}
