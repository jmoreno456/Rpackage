#' Train a regression model
#'
#' @param data A data frame.
#' @param target The name of the target variable as a string.
#'
#' @return A fitted model object.
#' @export
train_model <- function(data, target) {
  formula <- as.formula(paste(target, "~ ."))

  model_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

  fitted_model <- model_spec %>%
    fit(formula, data = data)

  return(fitted_model)
}
