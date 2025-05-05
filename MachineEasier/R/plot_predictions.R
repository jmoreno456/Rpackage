#' Plot Actual vs Predicted Values
#'
#' @param model A trained model object.
#' @param data A data frame containing the test data.
#' @param target The target variable (string).
#' @param point_color Color of points (default = "black").
#' @param point_size Size of points (default = 2).
#' @param custom_layers Additional ggplot2 layers (optional).
#' @param ... Additional arguments passed to geom_point().
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal
#' @export
plot_predictions <- function(model, data, target,
                             point_color = "black",
                             point_size = 2,
                             custom_layers = NULL,
                             ...) {
  predicted <- predict(model, newdata = data)
  actual <- data[[target]]

  plot_data <- data.frame(actual = actual, predicted = predicted)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = actual, y = predicted)) +
    ggplot2::geom_point(color = point_color, size = point_size, ...) +
    ggplot2::geom_smooth(method = "lm", color = "red") +
    ggplot2::labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
    ggplot2::theme_minimal()

  if (!is.null(custom_layers)) {
    p <- p + custom_layers
  }

  return(p)
}
