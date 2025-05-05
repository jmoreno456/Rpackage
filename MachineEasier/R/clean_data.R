#' Clean data by removing NA Values
#'
#' @param data A data frame
#' @param remove_na Logical. If TRUE, remove rows with NAs.
#' @return A cleaned data frame
#' @importFrom tidyr drop_na
#' @export

clean_data <- function(data, remove_na = TRUE) {
  if (remove_na) {
    data <- drop_na(data)
  }
  return(data)
}
