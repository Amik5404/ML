library(pROC)
library(dplyr)
library(caret)
library(readxl) 
library(ggplot2)
library(ConfusionTableR)

# Load your Excel dataset (replace 'your_dataset.xlsx' with your actual Excel file)
data <- read_excel('C:/Users/sherc/OneDrive/Desktop/Project 1/Code/data/BDHS_Diabetic_Data_Jahan.xlsx')

# Convert the diabetes to a factor 
data$diabetic <- as.factor(data$diabetic)
data$region <- as.numeric(factor(data$region))
data$residence <- as.numeric(factor(data$residence))
data$wealthIndex <- as.numeric(factor(data$wealthIndex))
data$education <- as.numeric(factor(data$education))
data$workingstatus <- as.numeric(factor(data$workingstatus))
data$smokingstatus <- as.numeric(factor(data$smokingstatus))
data$BMIgp <- as.numeric(factor(data$BMIgp))


# Scale the numeric features (important for logistic regression)
numeric_vars <- c("age", "armcircumference", "SBP", "DBP", "height", "weight")
data[numeric_vars] <- scale(data[numeric_vars])

set.seed(123) 
# Set seed for reproducibility 
splitIndex <- createDataPartition(data$diabetic, p = 0.7, list = FALSE, times = 1) 
train_data <- data[splitIndex, ] # 70% for training 
test_data <- data[-splitIndex, ] # 30% for testing 

# Calculate class weights based on the class imbalance
class_weights <- table(train_data$diabetic)
class_weights <- 1 / class_weights
names(class_weights) <- levels(train_data$diabetic)

# Create a vector of weights for each training instance
weights_vector <- class_weights[train_data$diabetic]

# Perform logistic regression with class weights
model <- glm(diabetic ~ ., data = train_data, family = "binomial", weights = weights_vector)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary (0 or 1) using a threshold (e.g., 0.5)
threshold <- 0.6
predicted_classes <- ifelse(predictions > threshold, "diabetes", "control")

predicted_classes <- factor(predicted_classes, levels = c("control", "diabetes"))

# Create a confusion matrix
confusion_matrix <- confusionMatrix(data = predicted_classes, reference = test_data$diabetic)

# Extract performance metrics
accuracy <- confusion_matrix$overall["Accuracy"]
sensitivity <- confusion_matrix$byClass["Sensitivity"]
precision <- confusion_matrix$byClass["Pos Pred Value"]
f1_score <- confusion_matrix$byClass["F1"]

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (Recall):", sensitivity, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Precision:", precision, "\n")

ConfusionTableR::binary_visualiseR(train_labels = predicted_classes,
                                   truth_labels = test_data$diabetic,
                                   class_label1 = "Control",
                                   class_label2 = "Diabetes",
                                   quadrant_col1 = "lightgreen",
                                   quadrant_col2 = "coral",
                                   custom_title = "Logistic Regression Confusion Matrix",
                                   text_col= "black",
                                   round_dig = 2,
                                   cm_stat_size = 1,  # Adjust the size of the matrix squares
                                   cm_stat_lbl_size = 1
                                   
)

?binary_visualiseR