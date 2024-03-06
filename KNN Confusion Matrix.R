library(caret)
library(dplyr)
library(readxl)
library(ggplot2)
library(class)
library(ROSE)
library(ConfusionTableR)

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

# Create a grid of hyperparameters to search (values of k and distance metrics)
k_values <- c(1, 3, 5, 7, 9)


# Perform k-Nearest Neighbors (KNN) classification with hyperparameter tuning
knn_tune <- train(
  formula,
  data = train_data,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(k= k_values)
)

# View the results of hyperparameter tuning
print(knn_tune)

best_k <- knn_tune$bestTune$k

# Perform k-Nearest Neighbors (KNN) classification with the best hyperparameters
knn_model <- train(
  formula,
  data = rbind(train_data, validation_data),
  method = "knn",
  tuneGrid = data.frame(k = best_k)
)

# Make predictions on the test data with the best hyperparameters
best_knn_predictions <- predict(knn_model, newdata = test_data)
# Calculate evaluation metrics using caret::confusionMatrix
confusion_matrix <- confusionMatrix(best_knn_predictions, test_data$diabetic)

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
# Plot the confusion matrix as a heatmap

ConfusionTableR::binary_visualiseR(train_labels = best_knn_predictions,
                                   truth_labels = test_data$diabetic,
                                   class_label1 = "Control",
                                   class_label2 = "Diabetes",
                                   quadrant_col1 = "lightgreen",
                                   quadrant_col2 = "coral",
                                   custom_title = "KNN Confusion Matrix",
                                   text_col= "black",
                                   round_dig = 2,
                                   cm_stat_size = 1,  # Adjust the size of the matrix squares
                                   cm_stat_lbl_size = 1
  
)

?binary_visualiseR