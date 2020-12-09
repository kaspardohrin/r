#install.packages(c("jqr", "jsonlite"))

library(jsonlite)

wish <- read.csv('wish_current.csv', encoding="UTF-8")

createUnitSoldLabelColumn <- function() {
  wish$unit_sold_label[(wish$units_sold > 1) & (wish$units_sold < 100)] <- 1
  wish$unit_sold_label[(wish$units_sold >= 100) & (wish$units_sold < 1000)] <- 2
  wish$unit_sold_label[(wish$units_sold >= 1000) & (wish$units_sold < 5000)] <- 3
  wish$unit_sold_label[wish$units_sold >= 5000] <- 4
}

createRatingLabelColumn <- function() {
  wish$rating_label[(wish$rating > 1) & (wish$rating < 3.540)] <- 1
  wish$rating_label[(wish$rating >= 3.540) & (wish$rating < 3.840)] <- 2
  wish$rating_label[(wish$rating >= 3.808) & (wish$rating < 4.100)] <- 3
  wish$rating_label[wish$rating >= 4.100] <- 4
}

# mean of rating label en quality label
wish$mean_rating_unitssold <- (wish$rating_label + wish$units_sold_label) /2

createRatingLabelColumn(wish)
createQualityLabelColumn(wish)

sub_selection <- head(wish)


i = 0
for(product in sub_selection$image_tags) {
  i <- i + 1;
  currentTaglist <- fromJSON(sub_selection$image_tags[i])
  sub_selection$image_tags_list[i] <- currentTaglist$result
}

i = 0
for(product in sub_selection$image_tags) {
  i <- i + 1;
  sub_selection$image_tags_list[[i]]$tag <- sub_selection$image_tags_list[[i]]$tag$en
}

# verplaats de tags een niveau hoger, i.e., $tags$en wordt $tags
sub_selection$new_row <- list(c(list(sub_selection$image_tags_list[[1]]$tag, sub_selection$image_tags_list[[1]]$confidence)))


#sub_selection$image_tags_list[[1]]$confidence
#sub_selection$image_tags_list[[1]]$tag$en

#length(sub_selection$image_tags_list[[1]]$confidence)


#for(j in 1) { #:length(sub_selection$image_tags_list)

  #collection <- list()
  
  #for (i in 1:length(sub_selection$image_tags_list[[j]][,1])) {
    #collection <- append(collection, list(c(sub_selection$image_tags_list[[j]]$confidence[i], sub_selection$image_tags_list[[j]]$tag$en[i])))
  #}
  #print(collection)
  #sub_selection$nieuwe_colom[j] <- df(collection)
#}



