#Loading required packages
# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('caret')
# install.packages('caretEnsemble')
# install.packages('psych')
# install.packages('Amelia')
# install.packages('mice')
# install.packages('GGally')
# install.packages('rpart')
# install.packages('randomForest')
# install.packages('e1071')

# https://www.edureka.co/blog/naive-bayes-in-r/

#library(tidyverse)
#library(ggplot2)
#library(caret)
#library(caretEnsemble)
#library(psych)
#library(Amelia)
#library(mice)
#library(GGally)
#library(rpart)
#library(randomForest)
library(e1071)

wish <- read.csv("wish/unitssold_scaled.csv")

wish <- na.omit(wish)

wish <- wish[,-1]

# nfold , x-validation -> methods to make sure good values weren't a coincidence
testIndexes <- sample(1:nrow(wish),as.integer(0.9*nrow(wish)))

test <- wish[-testIndexes, ]
train <- wish[testIndexes, ]

## Naive Bayes %6.6 accuracy
nb.model <- naiveBayes(as.factor(train$wish.unit_sold_label) ~ ., data=train)
prediction <- predict(nb.model, test, type="class")

# TODO: extract accuracy

accuracy <- 100 * sum(test$wish.unit_sold_label == prediction)/NROW(test$wish.unit_sold_label)
print(paste0("The accuracy of this model is: ", round(accuracy, digits=1)))

