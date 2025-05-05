#' Plot Actual vs Predicted Values
#'
#' This function generates a scatter plot comparing the actual vs predicted values for a fitted model. Optionally, it can add a regression line.
#'
#' @param model A trained model object.
#' @param data A data frame containing the test data.
#' @param target The name of the target variable (as a string).
#' @param point_color Color of the points (default is "black").
#' @param point_size Size of the points (default is 2).
#' @param custom_layers Additional `ggplot2` layers to be added to the plot (optional).
#' @param ... Additional arguments passed to `geom_point()` for customization.
#'
#' @return A `ggplot2` plot object.
#' @export
#'
#' @examples
#' model <- train_model(mtcars, "mpg", "lm")
#' plot_predictions(model, mtcars, "mpg")

plot_predictions <- function(model, data, target,
                             point_color = "black",
                             point_size = 2,
                             custom_layers = NULL,
                             ...) {
  predicted <- predict(model, newdata = data) %>%
    bind_cols(data)

  actual <- data[[target]]

  plot_data <- data.frame(actual = actual, predicted = predicted$.pred)

  p <- ggplot(plot_data, aes(x = actual, y = predicted)) +
    geom_point(color = point_color, size = point_size, ...) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
    theme_minimal()

  if (!is.null(custom_layers)) {
    p <- p + custom_layers
  }

  return(p)
}
