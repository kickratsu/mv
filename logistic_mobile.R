






















































# Load necessary libraries
library(dplyr)
library(nnet)
library(caret)

# Read the data
df <- read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\factor analsys\\mobileprice_train.csv")
#df1<-read.csv
# Convert relevant columns to factors
factor_cols <- c("blue", "dual_sim", "four_g", "three_g", "touch_screen", "wifi", "price_range")
df[factor_cols] <- lapply(df[factor_cols], factor)
dim(df)
# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- createDataPartition(df$price_range, p = 0.75, list = FALSE)
train_set <- df[train_index, ]
test_set <- df[-train_index, ]

dim(train_set)
dim(test_set)

# Fit multinomial logistic regression model
model <- multinom(price_range ~ ., data = train_set)

# Summary of the model
summary(model)

# Predicting on the test set
predictions <- predict(model, newdata = test_set, type = "class")#can replace test_set with test data

# Create confusion matrix
conf_matrix <- table(test_set$price_range, predictions)

# Print confusion matrix
cat("Confusion Matrix:\n")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print accuracy
cat("Accuracy on Test Data:", accuracy, "\n")

