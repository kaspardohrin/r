install.packages(c('httr', 'jsonlite', 'stringr'))

require('httr')
require('jsonlite')
require('stringr')

wish <- read.csv('wish_cleaned.csv', encoding="UTF-8")

api_key <- 'acc_59230ae5c7a1fac'
api_secret <- '812f824afbe14b9f3d5e10865cb79804'

images <- wish$product_picture

for (i in 1:length(images)) {
  picture <- wish$product_picture[i]
  
  result <- GET(url=sprintf('https://api.imagga.com/v2/tags?image_url=%s', picture), authenticate(api_key, api_secret), treshold="30.0")
  
  tags <- content(result, as = "text")
  
  wish$image_tags[i] <- tags
}

#wish$failed <- FALSE

#for (i in 1:length(images)) {
  #if (grepl('error', wish$image_tags[i], fixed=TRUE)) wish$failed[i] <- TRUE
#}
