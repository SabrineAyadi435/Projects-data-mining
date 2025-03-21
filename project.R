#downloading class packages
install.packages("class")
install.packages("dplyr")
install.packages("caret")
install.packages("pROC")
library(pROC)
library(class)
library(dplyr)
library(caret)
#imorting dataset
data <- read.csv("C:\\Users\\sabri\\Downloads\\archive (1)\\data.csv", header= TRUE)
head(data)
data <- data %>% select(-X)
# Encode the target variable as a factor
data$diagnosis <- as.factor(data$diagnosis)

# Remove the ID column
data <- data[, -1]

# Check the structure of the dataset
str(data)

# Step 3: Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Separate features and target variable
train_features <- train_data[, -1]
test_features <- test_data[, -1]
train_labels <- train_data$diagnosis
test_labels <- test_data$diagnosis

# Step 4: Normalize/Standardize the features
preprocess_params <- preProcess(train_features, method = c("center", "scale"))
train_features <- predict(preprocess_params, train_features)
test_features <- predict(preprocess_params, test_features)

# Step 5: Demonstrate Overfitting and Underfitting
# Define a range of k values
k_values <- seq(1, 50, by = 2)

# Initialize vectors to store accuracy
train_accuracy <- numeric(length(k_values))
test_accuracy <- numeric(length(k_values))

# Loop through k values and evaluate model performance
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Train the k-NN model
  knn_train_predictions <- knn(train = train_features, test = train_features, cl = train_labels, k = k)
  knn_test_predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = k)
  
  # Calculate accuracy on training and testing sets
  train_accuracy[i] <- sum(knn_train_predictions == train_labels) / length(train_labels)
  test_accuracy[i] <- sum(knn_test_predictions == test_labels) / length(test_labels)
}

# Step 6: Plot the results
results <- data.frame(k = k_values, Train_Accuracy = train_accuracy, Test_Accuracy = test_accuracy)
ggplot(results, aes(x = k)) +
  geom_line(aes(y = Train_Accuracy, color = "Training Accuracy")) +
  geom_line(aes(y = Test_Accuracy, color = "Testing Accuracy")) +
  labs(title = "Overfitting and Underfitting in k-NN",
       x = "Value of k",
       y = "Accuracy",
       color = "Legend") +
  theme_minimal()

# Step 7: Hyperparameter Tuning with an if-else loop
# Initialize variables to store the best k and its accuracy
best_k <- k_values[1]
best_accuracy <- test_accuracy[1]

# Loop through k values to find the best k
for (i in seq_along(k_values)) {
  k <- k_values[i]
  current_accuracy <- test_accuracy[i]
  
  # Update best k if current accuracy is better
  if (current_accuracy > best_accuracy) {
    best_k <- k
    best_accuracy <- current_accuracy
  }
}

# Print the best k and its accuracy
print(paste("Best k:", best_k))
print(paste("Best Testing Accuracy:", best_accuracy))
print(paste("Accuracy:", accuracy))
knn_predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = best_k)
onf_matrix <- confusionMatrix(knn_predictions, test_labels)
print(conf_matrix)

# Accuracy
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

# ROC-AUC
roc_curve <- roc(as.numeric(test_labels), as.numeric(knn_predictions))
auc_value <- auc(roc_curve)
print(paste("ROC-AUC:", auc_value))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for k-NN", col = "blue")