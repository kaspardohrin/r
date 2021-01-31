# mean of all labels label NOT ROUNDED
wish$mean_label <- (wish$rating_label + wish$unit_sold_label + wish$numberof_tags_label + wish$ratingof_merchant_label) / 4

# verplaats de tags een niveau hoger, i.e., $tags$en wordt $tags
# sub_selection$new_row <- list(c(list(sub_selection$image_tags_list[[1]]$tag, sub_selection$image_tags_list[[1]]$confidence)))

# test with only numbers 
wish_model_numbers <- data.frame(unit_sold=wish$unit_sold_label,
                                 rating_label = wish$rating_label,
                                 numberof_tags = wish$numberof_tags_label,
                                 ratingof_merchant = wish$ratingof_merchant_label)

# labels from 1-4 to 0-1
# wish_model_numbers$label <- wish_model_numbers$label/4
# wish_model_numbers$label <- round(wish_model_numbers$label)

# remove rows with na ?
wish_model_numbers <- na.omit(wish_model_numbers)

# add column with confidence level of first 10 tags in categories
for(i in 1:nrow(wish_model_numbers)) {
  wish_model_numbers$image_confidence[i] <- mean(wish$image_tags_list[[i]]$confidence[1:10])
}

# verdeel confidence in kwartielen
wish_model_numbers$image_confidence[(wish_model_numbers$image_confidence < 31.88)] <- 1
wish_model_numbers$image_confidence[(wish_model_numbers$image_confidence >= 31.88) & (wish_model_numbers$image_confidence < 37.34)] <- 2
wish_model_numbers$image_confidence[(wish_model_numbers$image_confidence >= 37.34) & (wish_model_numbers$image_confidence <= 44.97)] <- 3
wish_model_numbers$image_confidence[(wish_model_numbers$image_confidence > 44.97)] <- 4


wish_model_numbers <- subset(wish_model_numbers, select= -c(label))

# kNN
library(DMwR)
library(class)
library("imputeTS")

wish_df <- read.csv("/Users/ireneprins/wish_unitssold_df.csv")
wish_df$wish.has_urgency_banner <- na.replace(wish_df$wish.has_urgency_banner, 0)
wish_df_scale <- data.frame(scale(wish_df))
wish_df_scale <- na.omit(wish_df_scale)

idxs <- sample(1:nrow(wish_df_scale),as.integer(0.7*nrow(wish_df_scale)))

wish_df_scale.train <- wish_df_scale[idxs,]
wish_df_scale.test <- wish_df_scale[-idxs,]

cl <- factor(wish_df_scale.train$wish.unit_sold_label)

# prediction
nn3 <- knn(train= wish_df_scale.train, test = wish_df_scale.test, cl= cl, k=3)
nn5 <- knn(train= wish_df_scale.train, test = wish_df_scale.test, cl =cl, k=5)
nn7 <- knn(train= wish_df_scale.train, test = wish_df_scale.test, cl = cl, k=7)
nn10 <- knn(train= wish_df_scale.train, test = wish_df_scale.test, cl =cl, k=10)

acc.3 <- 100 * sum(wish_df_scale.test$wish.unit_sold_label == nn3)/NROW(wish_df_scale.test$wish.unit_sold_label) # 85.7
acc.5 <- 100 * sum(wish_df_scale.test$wish.unit_sold_label == nn5)/NROW(wish_df_scale.test$wish.unit_sold_label) # 83.2
acc.7 <- 100 * sum(wish_df_scale.test$wish.unit_sold_label == nn7)/NROW(wish_df_scale.test$wish.unit_sold_label) # 84.8
acc.10 <- 100 * sum(wish_df_scale.test$wish.unit_sold_label == nn10)/NROW(wish_df_scale.test$wish.unit_sold_label) # 81.0

plot(wish_df_scale)

