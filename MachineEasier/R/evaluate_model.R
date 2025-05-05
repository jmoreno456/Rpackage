#' Evaluate a Predictive Model
#'
#' @param model A trained model object.
#' @param data A data frame containing the test data.
#' @param target The target variable (string).
#' @return A list with RMSE and R² values.
#' importFrom caret RMSE R2
#' @export

evaluate_model <- function(model, data, target) {
  predicted <- predict(model, newdata = data)
  actual <- data[[target]]

  rmse <- RMSE(predicted, actual)
  r_squared <- R2(predicted, actual)

  return(list(RMSE = rmse, R2 = r_squared))
}
