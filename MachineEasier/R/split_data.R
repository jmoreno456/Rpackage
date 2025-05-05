#' Split Data into Training and Testing Sets
#'
#' This function splits a data frame into training and testing sets, with an option to specify the proportion of data to use for training.
#' The splitting ensures that the target variable is used for stratification if it is a factor or character column.
#'
#' @param data A data frame.
#' @param target The name of the target variable as a string.
#' @param prop Proportion of the data to be used for the training set (default is 0.8).
#'
#' @return A list containing the training and testing data frames.
#' @export
#'
#' @examples
#' split <- split_data(mtcars, "mpg")
#' train <- split$train
#' test <- split$test

split_data <- function(data, target, prop = 0.8) {
  if (!(target %in% colnames(data))) {
    stop("Target column not found in the data.")
  }

  if (!is.factor(data[[target]]) && !is.character(data[[target]])) {
    stop("Target column must be a factor or character vector.")
  }

  split <- initial_split(data, prop = prop, strata = target)
  list(
    train = training(split),
    test = testing(split)
  )
}
