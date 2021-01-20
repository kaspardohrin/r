#knn proberen

#install.packages("class")

library("class")

wish <- read.csv("unitssold_scaled.csv")

wish <- na.omit(wish)
wish <- wish[,-1]

mydata <- wish[sample(nrow(wish)),]
folds <- cut(seq(1,nrow(mydata)), breaks = 10, labels = FALSE)
for (i in 1:10){
  testindex <- which(folds==i, arr.ind = TRUE)
  test1 <- mydata[testindex,]
  train1 <- mydata[-testindex,] 
}

cl1 <- as.vector(train1$wish.unit_sold_label)

knn1 <- knn(train = train1, test = test1, cl = cl1, k = 1, l = 0, prob = FALSE, use.all = TRUE)
knn3 <- knn(train = train1, test = test1, cl = cl1, k = 3, l = 0, prob = FALSE, use.all = TRUE)
knn5 <- knn(train = train1, test = test1, cl = cl1, k = 5, l = 0, prob = FALSE, use.all = TRUE)
knn10 <- knn(train = train1, test = test1, cl = cl1, k = 10, l = 0, prob = FALSE, use.all = TRUE)

acc.1 <- 100 * sum(test1$wish.unit_sold_label == knn1)/NROW(test1$wish.unit_sold_label) # 90.07%
acc.3 <- 100 * sum(test1$wish.unit_sold_label == knn3)/NROW(test1$wish.unit_sold_label) # 87.42%
acc.5 <- 100 * sum(test1$wish.unit_sold_label == knn5)/NROW(test1$wish.unit_sold_label) # 86.09%
acc.10 <- 100 * sum(test1$wish.unit_sold_label == knn10)/NROW(test1$wish.unit_sold_label) # 84.11%

