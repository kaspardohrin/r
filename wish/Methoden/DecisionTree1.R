# DecisionTree manier 1
#source("wish_classification_GV1.R")
#source("wish_unitssold_df_GV1.R")
data <- wish_unitsold_df

mydata <- data[sample(nrow(data)),]
folds <- cut(seq(1,nrow(mydata)), breaks = 10, labels = FALSE)
for (i in 1:10){
  testindex <- which(folds==i, arr.ind = TRUE)
  test <- mydata[testindex,]
  train <- mydata[-testindex,] } 

library(rpart)
library(rpart.plot)
library(rattle)

fit <- rpart(
  train$wish.unit_sold_label~.,
  data = train,
  method = "class",
  minsplit = 2,
  minbucket = 3
)

prediction <- predict(fit, test[,-1], type = "class")
prop.table(table((test$wish.unit_sold_label==prediction)))*100

fancyRpartPlot(fit)


###########
# DecisionTree version manier 2
#source("wish_classification_GV1.R")
#source("wish_unitssold_df_GV1.R")

data2 <- wish_unitsold_df

data2$wish.price[data2$wish.price < summary(data2$wish.price)[[2]]] <- 1
data2$wish.price[(data2$wish.price >= summary(data2$wish.price)[[2]]) & (data2$wish.price < summary(data2$wish.price)[[4]])] <- 2
data2$wish.price[(data2$wish.price >=summary(data2$wish.price)[[4]]) & (data2$wish.price < summary(data2$wish.price)[[5]])] <- 3
data2$wish.price[data2$wish.price >= summary(data2$wish.price)[[5]]] <- 4

#summary(data2$wish.discount)
data2$wish.discount[data2$wish.discount < summary(data2$wish.discount)[[2]]] <- 1
data2$wish.discount[(data2$wish.discount >= summary(data2$wish.discount)[[2]]) & (data2$wish.discount < summary(data2$wish.discount)[[4]])] <- 2
data2$wish.discount[(data2$wish.discount >=summary(data2$wish.discount)[[4]]) & (data2$wish.discount < summary(data2$wish.discount)[[5]])] <- 3
data2$wish.discount[data2$wish.discount >= summary(data2$wish.discount)[[5]]] <- 4

#summary(data2$image_confidence)
data2$image_confidence[data2$image_confidence < summary(data2$image_confidence)[[2]]] <- 1
data2$image_confidence[(data2$image_confidence >= summary(data2$image_confidence)[[2]]) & (data2$image_confidence < summary(data2$image_confidence)[[4]])] <- 2
data2$image_confidence[(data2$image_confidence >=summary(data2$image_confidence)[[4]]) & (data2$image_confidence < summary(data2$image_confidence)[[5]])] <- 3
data2$image_confidence[data2$image_confidence >= summary(data2$image_confidence)[[5]]] <- 4

#summary(data2$wish.merchant_rating)
data2$wish.merchant_rating[data2$wish.merchant_rating < summary(data2$wish.merchant_rating)[[2]]] <- 1
data2$wish.merchant_rating[(data2$wish.merchant_rating >= summary(data2$wish.merchant_rating)[[2]]) & (data2$wish.merchant_rating < summary(data2$wish.merchant_rating)[[4]])] <- 2
data2$wish.merchant_rating[(data2$wish.merchant_rating >=summary(data2$wish.merchant_rating)[[4]]) & (data2$wish.merchant_rating < summary(data2$wish.merchant_rating)[[5]])] <- 3
data2$wish.merchant_rating[data2$wish.merchant_rating >= summary(data2$wish.merchant_rating)[[5]]] <- 4

mydata <- data2[sample(nrow(data2)),]
folds <- cut(seq(1,nrow(mydata)), breaks = 10, labels = FALSE)
for (i in 1:10){
  testindex <- which(folds==i, arr.ind = TRUE)
  test <- mydata[testindex,]
  train <- mydata[-testindex,] } 

fit <- rpart(
  train$wish.unit_sold_label~.,
  data = train,
  method = "class",
  minsplit = 40,
  minbucket = 8,
  cp= 0.001
)

prediction <- predict(fit, test[,-1], type = "class")
prop.table(table((test$wish.unit_sold_label==prediction)))*100

fancyRpartPlot(fit)


###########







