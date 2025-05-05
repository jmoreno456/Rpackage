#' Plot model predictions versus actual values
#'
#' This function creates a scatter plot of predicted values versus actual values for a regression model.
#' It ensures that the target variable is numeric before proceeding.
#'
#' @param model A fitted model object.
#' @param data A data frame containing the dataset.
#' @param target A string representing the name of the target (outcome) variable.
#'
#' @return A ggplot object.
#' @export
plot_predictions <- function(model, data, target) {
  # Ensure target is numeric for plotting
  if (!is.numeric(data[[target]])) {
    stop("For a regression model, the outcome should be numeric.")
  }

  # Generate predictions
  predictions <- predict(model, new_data = data)

  # Create ggplot of predictions
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[target]], y = predictions)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", color = "red") +
    ggplot2::labs(title = "Predictions vs Actuals", x = "Actual", y = "Predicted") +
    ggplot2::theme_minimal()
}
