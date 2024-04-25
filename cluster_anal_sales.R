





















































library(factoextra)
library(cluster)
library(NbClust)
data = read.csv("C:\\Users\\shriv\\Downloads\\mva sem 2\\Sales_Product_Details.csv")
str(data)
data = data[4:11]
str(data)
data$Product_Description = as.numeric(as.factor(data$Product_Description))
data$Product_Category = as.numeric(as.factor(data$Product_Category))
data$Product_Line = as.numeric(as.factor(data$Product_Line))
data$Raw_Material = as.numeric(as.factor(data$Raw_Material))
data$Region = as.numeric(as.factor(data$Region))
str(data)
# Standardize the data
df <- scale(data)
dim(df)
# Show the first 6 rows
head(df, nrow = 6)
# Compute the distance matrix

res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]

#identifying the strongest clustering structure
methods <- c("ward", "single", "complete", "average")
coefficients <- numeric(length(methods))
for (i in seq_along(methods)) {
  hc <- agnes(df, method = methods[i])
  coefficients[i] <- hc$ac
}
results <- data.frame(Methods = methods, Agglo_Coef = coefficients)
print(results)


# 1. Elbow Method
fviz_nbclust(data, FUN = hcut, method = "wss") + geom_vline(xintercept = 2,linetype = 2)
# 2. Silhouette Method
fviz_nbclust(data, FUN = hcut, method = "silhouette")


## Plot HAC
hac <- hclust(d = res.dist, method = "ward.D")
plot(hac, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hac, k = 2)

## Computing K-Means
set.seed(123)
km.res <- kmeans(x = df,centers = 2, nstart = 25)
#print(km.res)
km.res$cluster
km.res$centers
km.res$size

# Directly computing means using aggregate function
aggregate(x = data, by = list(cluster = km.res$cluster),FUN = mean)

## Adding Cluster Number to Data
dd <- cbind(data, cluster = km.res$cluster)
print(dd)

## Plot Cluster’s 
fviz_cluster(km.res, data = df, palette= "Set2", geom =  c("point", "text"), ggtheme = theme_minimal())
km.res$centers

