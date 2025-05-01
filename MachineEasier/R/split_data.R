#' Split data into training and test sets
#'
#' @param data A data frame.
#' @param target The target variable as a string.
#' @param prop The proportion of data to use for training.
#' @return A list with two data frames: $train and $test.
#' @export
split_data <- function(data, target, prop = 0.8) {
  # Make sure the target column exists
  if (!target %in% colnames(data)) {
    stop("Target variable not found in the data")
  }

  # Split the data into train and test sets
  set.seed(123)  # Ensure reproducibility
  n <- nrow(data)
  train_index <- sample(seq_len(n), size = floor(prop * n))

  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  list(train = train_data, test = test_data)
}
