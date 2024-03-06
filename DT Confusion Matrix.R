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


ConfusionTableR::binary_visualiseR(train_labels = final_predictions,
                                   truth_labels = test_data$diabetic,
                                   class_label1 = "Control",
                                   class_label2 = "Diabetes",
                                   quadrant_col1 = "lightgreen",
                                   quadrant_col2 = "coral",
                                   custom_title = "Decision Tree Confusion Matrix",
                                   text_col= "black",
                                   round_dig = 2,
                                   cm_stat_size = 1,  # Adjust the size of the matrix squares
                                   cm_stat_lbl_size = 1
                                   
)
