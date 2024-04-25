




















































library(corrr)
library(psych)
library(GPArotation)
library(factoextra)

data=read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\factor analsys\\ap_train.csv")


colSums(is.na(data))#checking null
df=na.omit(data)#removing null values records

str(df)

data=data[,9:22] #taking rating column only(index start from1)

# Calculate the correlation matrix
cor_matrix <- cor(data)

# Calculate eigenvalues
eigenvalues <- eigen(cor_matrix)$values

# Scree plot to visualize eigenvalues
plot(eigenvalues, type = "b", main = "Scree Plot")

#Factor score
out<-factanal(x=data,factors = 3,scores = "regression")
head(out$scores)


#Gives maximum weightage to loadings
f=factanal(data,3,rotation = "promax")

#Diagramatic representation of factors
loads<-f$loadings
fa.diagram(loads)


