#' Evaluate a Predictive Model
#'
#' @param model A trained model object.
#' @param data A data frame containing the test data.
#' @param target The target variable (string).
#' @return A list with RMSE and R² values.
#' @export

evaluate_model <- function(model, data, target) {
  predicted <- predict(model, newdata = data)
  actual <- data[[target]]

  rmse <- sqrt(mean((predicted - actual)^2))
  r_squared <- 1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2)

  return(list(RMSE = rmse, R2 = r_squared))
}
