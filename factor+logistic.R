


















































data <- read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\factor analsys\\ap_train.csv")

data_clean <- na.omit(data)
rated_values=data_clean[,9:21]
# correlation matrix
cor_matrix <- cor(rated_values)

# Calculate eigenvalues
eigenvalues <- eigen(cor_matrix)$values

# Scree plot to visualize eigenvalues
plot(eigenvalues, type = "b", main = "Scree Plot")

# Perform factor analysis with the determined number of factors
factor_model <- factanal(rated_values, factors = 3,scores = "regression")
factor_scores=factor_model$scores
factor_scores
df <- subset(data_clean, select = -c(Inflight.wifi.service, Departure.Arrival.time.convenient,
                             Ease.of.Online.booking, Gate.location, Food.and.drink,
                             Online.boarding, Seat.comfort, Inflight.entertainment,
                             On.board.service, Leg.room.service, Baggage.handling,
                             Checkin.service, Inflight.service, Cleanliness))

str(df)

# Assuming you already have 'df' with columns removed and 'factor_scores'

# Bind the factor scores with the subsetted dataframe
df_with_factor_scores <- cbind(df, factor_scores)
df_with_factor_scores$satisfaction = as.factor(df_with_factor_scores$satisfaction)
# View the resulting dataframe
print(df_with_factor_scores)
View(df_with_factor_scores)
str(df_with_factor_scores)

#########################
# Load necessary library
library(nnet)

# Fit multinomial logistic regression model
model <- multinom(satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + Class + Flight.Distance +
                    Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Factor1 + Factor2 + Factor3,
                  data = df_with_factor_scores)
# Summary of the model
summary(model)

### Testing Model ###
test_data <- read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\factor analsys\\ap_test.csv")
# Remove any missing values
data_clean <- na.omit(test_data)
rated_values=data_clean[,9:21]
factor_model <- factanal(rated_values, factors = 3,scores = "regression")
factor_scores=factor_model$scores
df <- subset(data_clean, select = -c(Inflight.wifi.service, Departure.Arrival.time.convenient,
                                     Ease.of.Online.booking, Gate.location, Food.and.drink,
                                     Online.boarding, Seat.comfort, Inflight.entertainment,
                                     On.board.service, Leg.room.service, Baggage.handling,
                                     Checkin.service, Inflight.service, Cleanliness))
df_with_factor_scores <- cbind(df, factor_scores)
df_with_factor_scores$satisfaction = as.factor(df_with_factor_scores$satisfaction)
# Predicting on the dataframe with factor scores
predictions <- predict(model, newdata = df_with_factor_scores, type = "class")
# Create confusion matrix
conf_matrix <- table(df_with_factor_scores$satisfaction, predictions)
# Print confusion matrix
cat("Confusion Matrix:\n")
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print accuracy
cat("Accuracy on Test Data:", accuracy, "\n")
