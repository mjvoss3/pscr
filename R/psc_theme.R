
#' PSC color scheme
#'
#' This function returns a specified number of colors (up to 10) in the PSC color scheme
#' @param num A number from 1 to 10
#' @return A character vector of PSC color hex codes
#' @export
psc_colors = function(num){
  if(num > 10){
    stop("Choose a number from 1-10")
  }

  # Colors in order: Dark blue, Dark red, Dark green, Dark yellow, Dark gray, Light blue, Light red, Light green, Light yellow, Light gray
  colors = c("#2E6A8F", "#B85030", "#687964", "#DFB134", "#6D6E71", "#8CAED0", "#D85A3C", "#93C13E", "#EDCE18", "#808285")
  colors[1:num]
}

#' ggplot2 theme for PSC
#'
#' A ggplot2 theme function based on theme_minimal() that changes other elements to PSC standards
#' @export
theme_psc <- function(){
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}
