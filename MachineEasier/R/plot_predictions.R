#' Plot Actual vs Predicted Values
#'
#' @param model A trained model object.
#' @param data A data frame containing the test data.
#' @param target The target variable (string).
#' @return A ggplot object showing the plot.
#' @export

plot_predictions <- function(model, data, target) {
  predicted <- predict(model, newdata = data)
  actual <- data[[target]]

  plot_data <- data.frame(actual = actual, predicted = predicted)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = actual, y = predicted)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", color = "red") +
    ggplot2::labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")
}
