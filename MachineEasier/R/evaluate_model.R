#' Evaluate Model Performance
#'
#' This function evaluates a fitted model's performance by calculating RMSE (Root Mean Squared Error) and R-squared using the test data.
#'
#' @param model A fitted model object created using the `fit()` function from the `parsnip` package.
#' @param test_data A data frame containing the test data.
#' @param target The name of the target column (as a string).
#'
#' @return A list containing RMSE and R-squared metrics.
#' @export
#'
#' @examples
#' model <- train_model(mtcars, "mpg", "lm")
#' metrics <- evaluate_model(model, test_data, "mpg")

evaluate_model <- function(model, test_data, target) {
  predictions <- predict(model, new_data = test_data) %>% bind_cols(test_data)

  metrics <- list(
    RMSE = rmse(predictions, truth = !!sym(target), estimate = .pred),
    R2   = rsq(predictions, truth = !!sym(target), estimate = .pred)
  )

  return(metrics)
}
