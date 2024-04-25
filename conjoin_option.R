

















































library(conjoint)

data=read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\pizza_data.csv")

data$price=as.numeric(as.factor(data$price))

str(data)
data

# Perform conjoint analysis
ca_model <- caModel(data[, "ranking"], data[, c("brand", "price", "weight", "crust", "cheese", "size", "toppings", "spicy")])
print(ca_model)
# Extract part-worth utilities
part_worth <- coef(ca_model)[-1, ]

# Calculate the absolute values of coefficients for each attribute
abs_part_worth <- abs(part_worth)

# Calculate the total importance of each attribute
attribute_importance <- rowSums(abs_part_worth)

# Create a data frame for easier plotting
importance_df <- data.frame(Attribute = rownames(abs_part_worth), Importance = attribute_importance)

# Sort the data frame by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
print(importance_df)


barplot(importance_df$Importance, main = "Relative Importance of Attributes", ylab = "Importance", col = rainbow(nrow(importance_df)))

# Generate legend
legend("topright", inset = 0.05, legend = importance_df$Attribute, fill = rainbow(nrow(importance_df)), title = "Attribute")
unique(data$brand)
unique(data$crust)
unique(data$weight)


# Perform linear regression

# Fit the linear regression model with all features
lm_model <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = pizza_data)

# Print the summary of lm_model
summary(lm_model)
