#' Train a Predictive Model
#'
#' This function trains a predictive model using the specified method ("lm" for linear regression or "rf" for random forest).
#' The trained model is returned as a fitted workflow object.
#'
#' @param data A data frame containing the training data.
#' @param target The name of the target variable as a string.
#' @param method The modeling method to use: "lm" for linear regression or "rf" for random forest (default is "lm").
#'
#' @return A fitted workflow object for the trained model.
#' @export
#'
#' @examples
#' model <- train_model(mtcars, "mpg", "lm")

train_model <- function(data, target, method = "lm") {
  formula <- as.formula(paste(target, "~ ."))

  model_spec <- switch(
    method,
    "lm" = linear_reg() %>% set_engine("lm") %>% set_mode("regression"),
    "rf" = rand_forest() %>% set_engine("ranger") %>% set_mode("regression"),
    stop("Unsupported method. Use 'lm' or 'rf'.")
  )

  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_formula(formula)

  fitted_model <- fit(wf, data = data)

  return(fitted_model)
}
