#' Evaluate a predictive model
#'
#' This function evaluates the performance of a regression model using RMSE and R-squared.
#' It ensures that the target variable is numeric before performing the evaluation.
#'
#' @param model A fitted model object.
#' @param data A data frame containing the dataset.
#' @param target A string representing the name of the target (outcome) variable.
#'
#' @return A list containing the RMSE and R-squared values.
#' @export
evaluate_model <- function(model, data, target) {
  # Ensure target is numeric for evaluation
  if (!is.numeric(data[[target]])) {
    stop("For a regression model, the outcome should be numeric.")
  }

  # Make predictions
  predictions <- predict(model, new_data = data)

  # Calculate RMSE and R2
  rmse <- caret::RMSE(predictions, data[[target]])
  r2 <- caret::R2(predictions, data[[target]])

  return(list(RMSE = rmse, R2 = r2))
}
