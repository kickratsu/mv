























































#read the data
data=read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\pizza_data.csv")
data$brand=as.numeric(as.factor(data$brand))
data$price=as.numeric(as.factor(data$price))
data$weight=as.numeric(as.factor(data$weight))
data$crust=as.numeric(as.factor(data$crust))
data$cheese =as.numeric(as.factor(data$cheese))
data$size =as.numeric(as.factor(data$size))
data$toppings =as.numeric(as.factor(data$toppings))
data$spicy=as.numeric(as.factor(data$spicy))
str(data)
data
#running conjoint analysis with regression
conj.result<-lm(ranking~.,data=data)
summary(conj.result)

library(conjoint)
#attribute levels
plevel = c('Dominos','Onesta','Oven Story','Pizza hut','$1.00','$2.00','$3.00','$4.00','100g','200g','300g','400g',
           'thick','thin','Cheddar','Mozzarella','large','regular','mushroom','paneer','extra','normal')
plevel_df = data.frame(plevel)
plevel_df

install.packages('conjoint')

#getting utilities of each attribute
caModel(y = data[,9], x = data[1:8] )
Conjoint(y = data[,9], x = data[1:8],z=plevel_df)
??Conjoint

#total utilities based on model
caTotalUtilities(data[,9],data[1:8])

#attribute importance
caImportance(data[,9],data[1:8])
# Run conjoint analysis for attribute importance
importance_result <- caImportance(data[, 9], data[, 1:8])

# Convert the result to a data frame
importance_df <- data.frame(Attribute = names(importance_result), Importance = importance_result)


