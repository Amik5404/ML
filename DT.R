library(dplyr)
library(caret)
library(readxl)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ROSE)

# Load your Excel dataset (replace 'your_dataset.xlsx' with your actual Excel file)
data <- read_excel('C:/Users/sherc/OneDrive/Desktop/Project 1/Code/data/BDHS_Diabetic_Data_Jahan.xlsx')

# Convert text selections to factors
data$region <- as.factor(data$region)
data$education <- as.factor(data$education)
data$BMIgp <- as.factor(data$BMIgp)
data$diabetic <- as.factor(data$diabetic)
data$residence <- as.factor(data$residence)
data$wealthIndex <- as.factor(data$wealthIndex)
data$workingstatus <- as.factor(data$workingstatus)
data$smokingstatus <- as.factor(data$smokingstatus)

# Define the columns to scale (numeric and decimal)
numeric_features <- c("age", "armcircumference", "SBP", "DBP", "height", "weight")
# Scale numeric and decimal columns
data[numeric_features] <- scale(data[numeric_features])


# Perform SMOTE to balance the classes
data_balanced <- ROSE(diabetic ~ ., data = data, seed = 123, N = 2 * table(data$diabetic)[["control"]], p = 0.5)$data

# Split the data into training (60%), validation (20%), and testing (20%) sets
set.seed(123)
train_index <- createDataPartition(data_balanced$diabetic, p = 0.7, list = FALSE)
validation_index <- createDataPartition(data_balanced[-train_index, ]$diabetic, p = 0.6, list = FALSE)
train_data <- data_balanced[train_index, ]
validation_data <- data_balanced[-train_index, ][validation_index, ]
test_data <- data_balanced[-train_index, ]


formula <- diabetic ~ .


tree_train <- train(
  formula,
  data = train_data,
  method = "rpart",
  cp = 0.001,
  minsplit = 5
)

# Create the final model using the best hyperparameters
final_tree_model <- rpart(
  formula,
  data = rbind(train_data, validation_data),  # Combine training and validation data
  method = "class",
  cp = 0.001,
  minsplit = 5
)


# Make predictions with the final model on the test data
final_predictions <- predict(final_tree_model, test_data, type = "class")

# Evaluate the final model
confusion_matrix <- confusionMatrix(final_predictions, test_data$diabetic)

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
  Value = c(accuracy, precision, recall, f1_score)
)

# Calculate percentages
percentage <- scales::percent(metrics_df$Value)

# Plot the metrics
ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", size = 0.5) +  # Add black outlines to bars
  geom_text(aes(label = percentage), vjust = -0.5, size = 3) +  # Add percentage labels above bars
  theme_minimal() +
  labs(title = "Model Evaluation Metrics", y = "Value") +
  scale_fill_brewer(palette = "Set1") +  # Use a different color palette
  ylim(0, 1) +  # Set the y-axis limits to 0-1 for clarity
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.position = "none")  # Remove the legend