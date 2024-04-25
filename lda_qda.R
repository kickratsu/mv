

















































library(MASS)
library(caret)
library(ROCR)

## Loading Data & Preprocessing
df = read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\BREAST.csv")
dim(df)
df = df[,2:32]
str(df)
colSums(is.na(df))
df$diagnosis = as.factor(df$diagnosis)
str(df)

## Checking for Outlier
b=boxplot(df)
## Making LDA Model
actual_lda = lda(diagnosis ~ . , data=df)
## Predicting 
predicted_lda = predict(actual_lda)
## Plotting Prediction
plot(predicted_lda$x, col = as.numeric(predicted_lda$class), pch = 19,
     xlab = "LD1", ylab = "LD2", main = "Scatter Plot of LDA")


## Adding legend
legend("topright", legend = levels(predicted_lda$class), col = c("black", "red"), pch = 19)

## Confusion Matrix
confusionMatrix(predicted_lda$class,df$diagnosis)$table
## Accuracy 
mean(predicted_lda$class==df$diagnosis)

##  Finding Prediction True Positive vs False Positive
pred_lda <- prediction(predicted_lda$posterior[, "M"], df$diagnosis == "M")
perf_lda <- performance(pred_lda, "tpr", "fpr")
## PLoting ROC Curve for LDA
plot(perf_lda, col = "blue", lwd = 2, main = "ROC Curve", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
## AUC Score
auc_lda <- performance(pred_lda, "auc")
print(paste("AUC for LDA:", auc_lda@y.values[[1]]))

## QDA Modle
actual_qda = qda(diagnosis ~ . , data=df)
## Prediction using Model
predicted_qda = predict(actual_qda)
## Accuracy
mean(predicted_qda$class==df$diagnosis)
## Confution Matrix
confusionMatrix(predicted_qda$class,df$diagnosis)$table
pred_qda <- prediction(predicted_qda$posterior[, "M"], df$diagnosis == "M")
# Create performance object for ROC curve
perf_qda <- performance(pred_qda, "tpr", "fpr")
# Plot ROC curve
plot(perf_qda, col = "red", lwd = 2, add = TRUE)
# Calculate AUC
auc_qda <- performance(pred_qda, "auc")
print(paste("AUC for QDA:", auc_qda@y.values[[1]]))
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)
