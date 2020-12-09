# mean of all labels label NOT ROUNDED
wish$mean_label <- (wish$rating_label + wish$unit_sold_label + wish$numberof_tags_label + wish$ratingof_merchant_label) / 4

# verplaats de tags een niveau hoger, i.e., $tags$en wordt $tags
# sub_selection$new_row <- list(c(list(sub_selection$image_tags_list[[1]]$tag, sub_selection$image_tags_list[[1]]$confidence)))

# create dataframe with only tags and mean label for machine learning
wish_model <- data.frame(mean_label=wish$mean_label, tagslist = I(wish$image_tags_list))

# test with only numbers 
wish_model_numbers <- data.frame(unit_sold=wish$unit_sold_label,
                                 rating_label = wish$rating_label,
                                 numberof_tags = wish$numberof_tags_label,
                                 ratingof_merchant = wish$ratingof_merchant_label, label = wish$mean_label)

# labels from 1-4 to 0-1
# wish_model_numbers$label <- wish_model_numbers$label/4

# remove rows with na ?
wish_model_numbers <- na.omit(wish_model_numbers)

# kNN
library(DMwR)
library(class)

wish_model_numbers$rounded_label_4[(wish_model_numbers$label <= 0.25)] <- 0
wish_model_numbers$rounded_label_4[(wish_model_numbers$label > 0.25) & (wish_model_numbers$label <= 0.5)] <- 1
wish_model_numbers$rounded_label_4[(wish_model_numbers$label > 0.5) & (wish_model_numbers$label <= 0.75)] <- 2
wish_model_numbers$rounded_label_4[(wish_model_numbers$label > 0.75)] <- 3

idxs <- sample(1:nrow(wish_model_numbers),as.integer(0.7*nrow(wish_model_numbers)))
wish_model_numbers.train <- wish_model_numbers[idxs,]
wish_model_numbers.test <- wish_model_numbers[-idxs,]

cl <- factor(wish_model_numbers.train$rounded_label_4)

# prediction
nn3 <- knn(train= wish_model_numbers.train, test = wish_model_numbers.test, cl= cl, k=3)
nn5 <- knn(train= wish_model_numbers.train, test = wish_model_numbers.test, cl =cl, k=5)
nn7 <- knn(train= wish_model_numbers.train, test = wish_model_numbers.test, cl = cl, k=7)
nn10 <- knn(train= wish_model_numbers.train, test = wish_model_numbers.test, cl =cl, k=10)

prediction <- data.frame(pred= nn3, real = wish_model_numbers.test$rounded_label_4)

acc.3 <- 100 * sum(wish_model_numbers.test$rounded_label_4 == nn3)/NROW(wish_model_numbers.test$rounded_label_4) # 100%
acc.5 <- 100 * sum(wish_model_numbers.test$rounded_label == nn5)/NROW(wish_model_numbers.test$rounded_label) # 100%
acc.7 <- 100 * sum(wish_model_numbers.test$rounded_label == nn7)/NROW(wish_model_numbers.test$rounded_label) # 100%
acc.10 <- 100 * sum(wish_model_numbers.test$rounded_label == nn10)/NROW(wish_model_numbers.test$rounded_label) # 100%


