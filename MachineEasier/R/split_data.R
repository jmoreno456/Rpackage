#' Split the dataset into training and testing sets
#'
#' This function splits the dataset into training and testing sets. The split is stratified based on the target variable.
#' It uses the updated `select()` function to avoid deprecation issues.
#'
#' @param data A data frame containing the dataset.
#' @param target A string representing the name of the target (outcome) variable.
#' @param test_size A numeric value between 0 and 1 indicating the proportion of data to be used for testing (default is 0.2).
#'
#' @return A list containing two data frames: `train` and `test`.
#' @export
split_data <- function(data, target, test_size = 0.2) {
  # Ensure target is a valid column
  if (!(target %in% colnames(data))) {
    stop("Target variable not found in the dataset.")
  }

  # Update select() to use all_of() to avoid deprecated behavior
  data_split <- rsample::initial_split(data, prop = 1 - test_size, strata = dplyr::all_of(target))

  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  return(list(train = train_data, test = test_data))
}
