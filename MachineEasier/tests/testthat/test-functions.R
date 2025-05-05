test_that("train_model returns a model object", {
  data <- mtcars
  target <- "mpg"
  model <- train_model(data, target)
  expect_s3_class(model, "model_fit")
})

test_that("evaluate_model returns RMSE and R2", {
  data <- mtcars
  target <- "mpg"
  model <- train_model(data, target)
  metrics <- evaluate_model(model, data, target)
  expect_named(metrics, c("RMSE", "R2"))
  expect_type(metrics$RMSE, "double")
  expect_type(metrics$R2, "double")
})

test_that("evaluate_model errors on non-numeric target", {
  data <- mtcars
  data$cyl <- as.character(data$cyl)
  expect_error({
    model <- train_model(data, "cyl")
    evaluate_model(model, data, "cyl")
  })
})

test_that("plot_predictions returns a ggplot object", {
  data <- mtcars
  target <- "mpg"
  model <- train_model(data, target)
  p <- plot_predictions(model, data, target)
  expect_s3_class(p, "ggplot")
})

test_that("plot_predictions errors on non-numeric target", {
  data <- mtcars
  data$cyl <- as.character(data$cyl)
  expect_error({
    model <- train_model(data, "cyl")
    plot_predictions(model, data, "cyl")
  })
})

test_that("split_data returns train and test data", {
  data <- mtcars
  target <- "mpg"
  split <- split_data(data, target)
  expect_named(split, c("train", "test"))
  expect_s3_class(split$train, "data.frame")
  expect_s3_class(split$test, "data.frame")
})
