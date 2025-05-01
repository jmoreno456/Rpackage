# Load the package and the iris dataset
library(MachineEasier)
data(iris)

# Split the data
split_iris <- split_data(iris, target = "Species", prop = 0.8)

# Train a model using linear regression (assuming the target variable is numeric for simplicity)
model_iris <- train_model(split_iris$train, target = "Sepal.Length", method = "lm")

# Evaluate the model
model_evaluation <- evaluate_model(model_iris, split_iris$test, target = "Sepal.Length")
print(model_evaluation)

# Visualize predictions
plot_predictions(model_iris, split_iris$test, target = "Sepal.Length")