#' Train a Predictive Model
#'
#' @param data A data frame with training data.
#' @param target The target variable string.
#' @param method The model method ("lm", "rf", "svm")
#' @return A trained model object.
#' @export

train_model <- function(data, target, method = "lm") {
  formula <- as.formula(paste(target, "~ ."))

  if (method == "lm") {
    model <- lm(formula, data = data)
  } else if (method == "rf") {
    model <- randomForest::randomForest(formula, data = data)
  } else if (method == "svm") {
    model <- e1071::svm(formula, data = data)
  } else {
    stop("Unsupported model method")
  }

  return(model)
}
