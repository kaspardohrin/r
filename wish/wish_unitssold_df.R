wish_unitsold_df <- data.frame(wish$unit_sold_label, wish$price, wish$retail_price, wish$numberof_tags_label, wish$uses_ad_boosts, wish$shipping_is_express, wish$merchant_rating, wish$badge_fast_shipping, wish$badge_local_product, wish$badge_product_quality, wish$has_urgency_banner)

wish_unitsold_df$wish.retail_price <- wish_unitsold_df$wish.retail_price - wish_unitsold_df$wish.price
colnames(wish_unitsold_df)[which(names(wish_unitsold_df) == "wish.retail_price")] <- "wish.discount"

# mean image confidence moet nog idk meer hoe we dat hebben gedaan
for(i in 1:nrow(wish_unitsold_df)) {
  wish_unitsold_df$image_confidence[i] <- mean(wish$image_tags_list[[i]]$confidence[1:10])
}
