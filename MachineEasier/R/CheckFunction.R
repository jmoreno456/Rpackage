#' Check if functions are loaded correctly
#'
#' @export
check_functions <- function() {
  # List of functions to check
  required_functions <- c("split_data", "train_model", "evaluate_model", "plot_predictions")

  # Check if each function is available
  missing_functions <- setdiff(required_functions, ls(getNamespace("MachineEasier)))

  if (length(missing_functions) == 0) {
    message("All functions are loaded correctly.")
  } else {
    message("Missing functions: ", paste(missing_functions, collapse = ", "))
  }
}
