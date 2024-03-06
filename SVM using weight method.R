# Load necessary libraries
library(e1071)  # For SVM
library(caret)   # For evaluation metrics and preprocessing
library(ggplot2) # For plotting
library(readxl)
library(plotly)
library(scales)  # For percentage formatting

# Load your Excel dataset (replace 'your_dataset.xlsx' with your actual Excel file)
data <- read_excel('C:/Users/sherc/OneDrive/Desktop/Project 1/Code/data/BDHS_Diabetic_Data_Jahan.xlsx')

# Convert the diabetic column to a factor
data$diabetic <- as.factor(data$diabetic)

# Define the columns to scale
columns_to_scale <- c(
  "height", "weight", "armcircumference", "SBP", "DBP", "age"
)

# Scale the selected columns using scale()
data[columns_to_scale] <- scale(data[columns_to_scale])

# Split the data into training (60%), validation (20%), and testing (20%) sets
set.seed(123)
train_index <- createDataPartition(data$diabetic, p = 0.7, list = FALSE)
validation_index <- createDataPartition(data[-train_index, ]$diabetic, p = 0.5, list = FALSE)
train_data <- data[train_index, ]
validation_data <- data[-train_index, ][validation_index, ]
test_data <- data[-train_index, ][-validation_index, ]

# Calculate class weights based on the class imbalance
class_weights <- table(train_data$diabetic)
class_weights <- 1 / class_weights
names(class_weights) <- levels(train_data$diabetic)

# Define a formula for the SVM model
formula <- diabetic ~ .

# Define a grid of hyperparameters for tuning
hyperparameters <- expand.grid(
  C = c(0.01, 0.1, 1, 10, 100),
  sigma = c(0.01, 0.1, 1, 10))


# Tune hyperparameters using grid search and 5-fold cross-validation on the validation set
svm_tune <- train(
  formula,
  data = train_data,
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyperparameters,
  class.weights = class_weights
)

# Get the best hyperparameters
best_cost <- svm_tune$bestTune$C
best_sigma <- svm_tune$bestTune$sigma

# Train the SVM model with the best hyperparameters on the combined training and validation sets
svm_model <- svm(
  formula,
  data = rbind(train_data, validation_data),
  cost = best_cost,
  sigma = best_sigma,
  class.weights = class_weights
)


# Make predictions on the test set
test_predictions <- predict(svm_model, newdata = test_data)

# Calculate evaluation metrics using caret::confusionMatrix
confusion_matrix <- confusionMatrix(test_predictions, test_data$diabetic)

# Extract metrics
accuracy <- confusion_matrix$overall["Accuracy"]
precision <- confusion_matrix$byClass["Pos Pred Value"]
recall <- confusion_matrix$byClass["Recall"]
f1_score <- confusion_matrix$byClass["F1"]

# Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1", f1_score, "\n" )

# Create a data frame for plotting
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1"),
  Value = c(accuracy, precision, recall,f1_score )
)

# Plot the metrics
ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", size = 0.5) +  # Add black outlines to bars
  theme_minimal() +
  labs(title = "Model Evaluation Metrics", y = "Value") +
  scale_fill_brewer(palette = "Set1") +  # Use a different color palette
  ylim(0, 1) +  # Set the y-axis limits to 0-1 for clarity
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.position = "bottom")  # Remove the legend



