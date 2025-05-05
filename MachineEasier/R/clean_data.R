#' Clean Data by Removing NA Values
#'
#' This function cleans a data frame by removing rows with missing values (NA) if the `remove_na` parameter is set to TRUE.
#'
#' @param data A data frame.
#' @param remove_na Logical. If TRUE, rows containing NA values are removed. Default is TRUE.
#'
#' @return A cleaned data frame with or without NA values, depending on the `remove_na` argument.
#' @export
#'
#' @examples
#' cleaned_data <- clean_data(mtcars)

clean_data <- function(data, remove_na = TRUE) {
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  if (remove_na) {
    data <- drop_na(data)
  }

  return(data)
}
