# Load the package and the mtcars dataset
library(MachineEasier)
data(mtcars)

# Split the data into training and test sets
split_cars <- split_data(mtcars, target = "mpg", prop = 0.8)

# Train a linear model using the training data
model_cars <- train_model(split_cars$train, target = "mpg", method = "lm")

# Evaluate the model on the test set
model_evaluation <- evaluate_model(model_cars, split_cars$test, target = "mpg")

# Print the evaluation results
print(model_evaluation)

# Plot the predictions
plot_predictions(model_cars, split_cars$test, target = "mpg")
