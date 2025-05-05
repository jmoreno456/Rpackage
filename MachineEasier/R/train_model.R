#' Train a Predictive Model with Cross-validation
#'
#' @param data A data frame with training data.
#' @param target The target variable string.
#' @param method The model method ("lm", "rf", "svm")
#' @param cv_folds The number of folds for cross-validation (default is 5).
#' @return A trained model object.
#' @importFrom e1071 svm
#' @importFrom rsample vfold_cv
#' @importFrom parsnip fit
#' @export

train_model <- function(data, target, method = "lm", cv_folds = 5) {
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # Ensure target exists in the data
  if (!target %in% colnames(data)) {
    stop("The target variable does not exist in the data.")
  }

  # Ensure method is one of "lm", "rf", or "svm"
  if (!(method %in% c("lm", "rf", "svm"))) {
    stop("Invalid method. Choose one of 'lm', 'rf', or 'svm'.")
  }

  # Create the formula for model fitting
  formula <- as.formula(paste(target, "~ ."))

  # Define resampling method
  resamples <- vfold_cv(data, v = cv_folds)

  # Train the model based on the chosen method
  if (method == "lm") {
    model <- fit(formula, data = data, method = "lm")
  } else if (method == "rf") {
    model <- fit(formula, data = data, method = "rf")
  } else if (method == "svm") {
    model <- svm(formula, data = data)
  }

  return(model)
}
