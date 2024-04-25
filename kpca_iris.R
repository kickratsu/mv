















































install.packages("kernlab")
library(kernlab)
data(iris)
test <- sample(1:150,20)
print(test)
kpc <- kpca(~.,data=iris[-test,-5],kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kpc)

#plot the data projection on the components
plot(rotated(kpc),col=as.integer(iris[-test,5]),
     xlab="1st Principal Component",ylab="2nd Principal Component")

#embed remaining points 
emb <- predict(kpc,iris[test,-5])
points(emb,col=c("Grey","Dark Red","Dark Green"),pch=25, cex=1)

legend("topleft", legend = c("Setosa", "Versicolor", "Virginica"), 
       col = c("Grey", "Dark Red", "Dark Green"), pch = 25, cex = 1)
