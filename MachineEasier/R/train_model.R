#' Train a Predictive Model
#'
#' @param data A data frame with training data.
#' @param target The target variable string.
#' @param method The model method ("lm", "rf", "svm")
#' @return A trained model object.
#' @importFrom e1071 svm
#' @export

train_model <- function(data, target, method = "lm") {
  # ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # ensure target exists in the data
  if (!target %in% colnames(data)) {
    stop("The target variable does not exist in the data.")
  }

  # ensure method is one of "lm", "rf", or "svm"
  if (!(method %in% c("lm", "rf", "svm"))) {
    stop("Invalid method. Choose one of 'lm', 'rf', or 'svm'.")
  }

  # Create the formula for model fitting
  formula <- as.formula(paste(target, "~ ."))

  # Train the model based on the chosen method
  if (method == "lm") {
    model <- lm(formula, data = data)
  } else if (method == "rf") {
    model <- randomForest::randomForest(formula, data = data)
  } else if (method == "svm") {
    model <- svm(formula, data = data)
  }

  return(model)
}
