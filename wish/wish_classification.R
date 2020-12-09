#install.packages(c("jqr", "jsonlite"))
#install.packages("stringr")
install.packages("readr")

library(stringr)
library(jsonlite)
library(readr)

wish <- read.csv("wish_current.csv")

# unitssold
createUnitSoldLabelColumn <- function() {
  wish$unit_sold_label[(wish$units_sold > 1) & (wish$units_sold < 100)] <- 1
  wish$unit_sold_label[(wish$units_sold >= 100) & (wish$units_sold < 1000)] <- 2
  wish$unit_sold_label[(wish$units_sold >= 1000) & (wish$units_sold < 5000)] <- 3
  wish$unit_sold_label[wish$units_sold >= 5000] <- 4
}

#rating
createRatingLabelColumn <- function() {
  wish$rating_label[(wish$rating > 1) & (wish$rating < 3.540)] <- 1
  wish$rating_label[(wish$rating >= 3.540) & (wish$rating < 3.840)] <- 2
  wish$rating_label[(wish$rating >= 3.808) & (wish$rating < 4.100)] <- 3
  wish$rating_label[wish$rating >= 4.100] <- 4
}

# Labelen uitkomsten van TAGS (Verdeelt in kwartielen) 
wish$numberof_tags_label <- 1+ str_count(wish$tags, "," )

wish$numberof_tags_label[(wish$numberof_tags_label < 14)] <- 3
wish$numberof_tags_label[(wish$numberof_tags_label >= 14) & (wish$numberof_tags < 17)] <- 2
wish$numberof_tags_label[(wish$numberof_tags_label >= 17) & (wish$numberof_tags < 20)] <- 4
wish$numberof_tags_label[wish$numberof_tags_label >= 20] <- 1


wish$ratingof_merchant_label[wish$merchant_rating < 3.915   ] <- 1
wish$ratingof_merchant_label[wish$merchant_rating >= 3.915 & wish$merchant_rating < 4.039   ] <- 2
wish$ratingof_merchant_label[wish$merchant_rating >= 4.039  & wish$merchant_rating < 4.155  ] <- 4
wish$ratingof_merchant_label[wish$merchant_rating >= 4.155] <- 3


# mean of all labels label NOT ROUNDED
wish$mean_label <- (wish$rating_label + wish$unit_sold_label + wish$numberof_tags_label + wish$ratingof_merchant_label) / 4

# create tag dataframe in wish data frame
i = 0
for(product in wish$image_tags) {
  i <- i + 1;
  currentTaglist <- fromJSON(wish$image_tags[i])
  wish$image_tags_list[i] <- currentTaglist$result
  wish$image_tags_list[[i]]$tag <- wish$image_tags_list[[i]]$tag$en
}

# verplaats de tags een niveau hoger, i.e., $tags$en wordt $tags
sub_selection$new_row <- list(c(list(sub_selection$image_tags_list[[1]]$tag, sub_selection$image_tags_list[[1]]$confidence)))

#create dataframe with only tags and mean label for machine learning
wish_model <- data.frame(mean_label=wish$mean_label, tagslist = I(wish$image_tags_list))





