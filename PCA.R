















































library(readxl)
library("FactoMineR")
library(ggcorrplot)
library('corrr')
library(factoextra)

AppleData <- read.csv("C:\\Users\\shriv\\Downloads\\apple_quality (1).csv")
str(AppleData)
df=AppleData[,2:8]
df$Acidity=as.numeric(df$Acidity)
df=na.omit(df)
colSums(is.na(df))
str(df)

## STEP 1
data_normalized <- scale(df)
head(data_normalized)
## STEP 2 PLOT COR GRAPH
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
## STEP 3
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]
## SCREE PLOT
fviz_eig(data.pca, addlabels = TRUE)
## Graph of the variables
fviz_pca_var(data.pca, col.var = "black")
## Contribution of each variable 
fviz_cos2(data.pca, choice = "var", axes = 1:2)
## Biplot combined with cos2 
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

