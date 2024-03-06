library(pROC)
library(dplyr)
library(caret)
library(readxl) 
library(ggplot2)

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
threshold <- 0.7
predicted_classes <- ifelse(predictions > threshold, "diabetes", "control")

predicted_classes <- factor(predicted_classes)

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