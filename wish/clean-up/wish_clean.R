install.packages(c('stringr'))

library(stringr)

# clothing sizes clean-up
# getting ready
wish <- read.csv("archive/summer-products-with-rating-and-performance_2020-08.csv", encoding="UTF-8")  # adjust to

colnames(wish)[colnames(wish) == "product_variation_size_id"] <- "sizes"
sizes <- wish$sizes

wish$sizes <- toupper(wish$sizes)
wish$sizes <- gsub(" ","",wish$sizes) # Removing spaces
wish$sizes <- gsub("-","",wish$sizes) # Removing symbols

# adjusting sizes by removing excess string from column "sizes"
wish$sizes <- str_remove(wish$sizes, "CHOOSEASIZE")
wish$sizes <- str_remove(wish$sizes, "SIZE")
wish$sizes <- str_remove(wish$sizes, "/")
wish$sizes <- str_remove(wish$sizes, "\\.")
wish$sizes <- str_remove(wish$sizes, "SUIT")
wish$sizes <- str_remove(wish$sizes, "\\(")  # functies werken niet bij objecten met tekens "(" of ")"
wish$sizes <- str_remove(wish$sizes, "\\)")  # functies werken niet bij objecten met tekens "(" of ")"

# adjusting sizes based on tags
socks <- grep("Socks", wish$tags)
wish[socks, "sizes"] <- "ONE SIZE"

scarf <- grep("scarf", wish$tags)
wish[scarf, "sizes"] <- "ONE SIZE"

# Adjusting sizes based on title
name1 <- grep("Nouveau Sexy Femmes Bandage Découpé Débardeurs Dames D'été Bustier Crop Tops Une Taille", wish$title)
wish[name1, "sizes"] <- "ONE SIZE"

name2 <- grep("Nouveau Mode Femmes Dames Crochet Kimono Tops Manteau Creux", wish$title)
wish[name2, "sizes"] <- "ONE SIZE"

name3 <- grep("Mode Femmes Blanc D'été Boho Sexy Dentelle Creuse Tricot Bikini Maillots De Bain Couvrir Crochet Plage Mini Robe Tops Blouse", wish$title)
wish[name3, "sizes"] <- "ONE SIZE"

name4 <- grep("Mode Femmes Fitness Sous-vêtements Sexy Camisole Rembourré Débardeurs Lady Floral Dentelle Bralette Crop Top", wish$title)
wish[name4, "sizes"] <- "S"

# adjustments to sizes in column "sizes"
wish$sizes <- gsub("1PCXL","XL", wish$sizes)
wish$sizes <- gsub("PANTS","",wish$sizes) # Removing spaces
wish$sizes <- gsub("45YEARS","S",wish$sizes)  # In tag staat maat "S"
#wish$sizes <- gsub("BUST88CM","",wish$sizes)
wish$sizes <- gsub("US","",wish$sizes)
wish$sizes <- gsub("32L","L",wish$sizes)

wish$sizes <- gsub("04XXXL","XXXXL",wish$sizes) #Code wordt soms niet gepakt, daarom 2x
wish$sizes <- gsub("2XL","XXL",wish$sizes)
wish$sizes <- gsub("3XL","XXXL",wish$sizes)
wish$sizes <- gsub("4XL","XXXXL",wish$sizes)
wish$sizes <- gsub("5XL","XXXXL",wish$sizes)
wish$sizes <- gsub("6XL","XXXXXXL",wish$sizes) # aantal unique values veranderen niet (eerste 6XL size)
wish$sizes <- gsub("04XXXL","XXXXL",wish$sizes) #Code wordt soms niet gepakt, daarom 2x

# Changes from numeric to alpha sizes based of website https://www.surdyke.com/SizeChart.htm
wish$sizes <- gsub("29","M",wish$sizes)
wish$sizes <- gsub("25","XS",wish$sizes)

wish$sizes <- gsub("26WAIST72CM28INCH","S", wish$sizes)
wish$sizes <- gsub("SWAIST5862CM","XS",wish$sizes)

#REMOVING NON-CLOTHEs items
wish  <-wish[!(wish$sizes=="AUPLUGLOWQUALITY"),] #
wish  <-wish[!(wish$sizes=="1MBY3M"),] # SUNSHADE/ZONNEBRIL
wish  <-wish[!(wish$sizes=="WHITE"),] # USB FAN
wish  <-wish[!(wish$sizes=="1PC"),] # Solar Floating Water Fountain
wish  <-wish[!(wish$sizes=="1"),] # Sizes unclear
wish  <-wish[!(wish$sizes=="FIRSTGENERATION"),] # Portable Mini Hanging Neck Fan
wish  <-wish[!(wish$sizes=="DAUGHTER24M"),] # Sizes unclear
wish  <-wish[!(wish$sizes=="BASECOAT"),] # Nail Art Gel Vanrish
wish  <-wish[!(wish$sizes=="BABYFLOATBOAT"),] # Steering Wheel
wish  <-wish[!(wish$sizes=="10ML"),] # Hair removal spray
wish  <-wish[!(wish$sizes=="17"),] # Mask
wish  <-wish[!(wish$sizes=="80X200CM"),] # Insectnet
wish  <-wish[!(wish$sizes=="5"),] # Ring / Jewelry

# Removing rows based on tags
Sandals <- grep("Sandals", wish$tags) # identifying rows with tag "Sandals"
wish <- wish[-c(Sandals),] #removing rows containing tag "Sandals"

inflatable <- grep("inflatable", wish$tags) # identifying rows with tag "inflatable"
wish <- wish[-c(inflatable),] #removing rows containing tag "inflatable"

Waterproof <- grep("Waterproof", wish$tags) # identifying rows with tag "Waterproof"
wish <- wish[-c(Waterproof),] #removing rows containing tag "Waterproof" (items: stickers, bag, blanket & phone case)

divingmask <- grep("divingmask", wish$tags) # identifying rows with tag "divingmask"
wish <- wish[-c(divingmask),] #removing rows containing tag "Waterproof" (items: stickers, bag, blanket & phone case)

sweatpad <- grep("sweatpad", wish$tags)
wish <- wish[-c(sweatpad),]

nail_kit<- grep("nail kit", wish$tags)
wish <- wish[-c(nail_kit),]

shoes <- grep("Shoes", wish$tags) # identifying rows with tag "shoes"
wish <- wish[-c(shoes),] #removing rows containing tag "shoes"

beachbag <- grep("beachbag", wish$tags) # identifying rows with tag "beachbag"
wish <- wish[-c(beachbag),] #removing rows containing tag "beachbag"

hats <- grep("Hats", wish$tags) # identifying rows with tag "Hat"
wish <- wish[-c(hats),] #removing rows containing tag "hats"

toy <- grep("Toy", wish$tags) # identifying rows with tag "Toy"  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
wish <- wish[-c(toy),] #removing rows containing tag "Toy"

ankletchain <- grep("ankletchain", wish$tags) # identifying rows with tag "ankletchain"
wish <- wish[-c(ankletchain),] #removing rows containing tag "ankletchain"

Necklace <- grep("Necklace", wish$tags) # identifying rows with tag "Necklace"
wish <- wish[-c(Necklace),] #removing rows containing tag "Necklace"

# removing excess characters after the right size
wish$sizes <- gsub("(S).*", "\\1", wish$sizes)
wish$sizes <- gsub("ONE S", "ONE SIZE", wish$sizes)

# removing rows based on title
name0 <- grep("Femmes Sexy dentelle Patchwork col en V Col en V Robe de plage d'été sans dos Mini-robe", wish$title)
wish <- wish[-c(name0),] #removing rows containing name

wish$image_tags[1:1510] <- 'null'

wish$product_color[wish$product_color == "Blue"] <- "blue"
wish$product_color[wish$product_color == "prussianblue"] <- "blue"
wish$product_color[wish$product_color == "denimblue"] <- "blue"
wish$product_color[wish$product_color == "navy blue"] <- "blue"
wish$product_color[wish$product_color == "darkblue"] <- "blue"
wish$product_color[wish$product_color == "lakeblue"] <- "blue"
wish$product_color[wish$product_color == "navy"] <- "blue"
wish$product_color[wish$product_color == "navyblue"] <- "blue"
wish$product_color[wish$product_color == "skyblue"] <- "blue"
wish$product_color[wish$product_color == "lightblue"] <- "blue"
wish$product_color[wish$product_color == "blue & pink"] <- "blue"
wish$product_color[wish$product_color == "navyblue & white"] <- "blue"
wish$product_color[wish$product_color == "skyblue"] <- "blue"

wish$product_color[wish$product_color == "White"] <- "white"
wish$product_color[wish$product_color == "offwhite"] <- "white"
wish$product_color[wish$product_color == "whitestripe"] <- "white"
wish$product_color[wish$product_color == "blackwhite"] <- "white"
wish$product_color[wish$product_color == "white & red"] <- "white"
wish$product_color[wish$product_color == "white & green"] <- "white"
wish$product_color[wish$product_color == "white & black"] <- "white"

wish$product_color[wish$product_color == "RED"] <- "red"
wish$product_color[wish$product_color == "Rose red"] <- "red"
wish$product_color[wish$product_color == "Rose red"] <- "red"
wish$product_color[wish$product_color == "coralred"] <- "red"
wish$product_color[wish$product_color == "rosered"] <- "red"
wish$product_color[wish$product_color == "rose"] <- "red"
wish$product_color[wish$product_color == "lightred"] <- "red"
wish$product_color[wish$product_color == "wine red"] <- "red"
wish$product_color[wish$product_color == "wine"] <- "red"
wish$product_color[wish$product_color == "claret"] <- "red"
wish$product_color[wish$product_color == "burgundy"] <- "red"
wish$product_color[wish$product_color == "winered & yellow"] <- "red"
wish$product_color[wish$product_color == "whinered"] <- "red"
wish$product_color[wish$product_color == "red & blue"] <- "red"
wish$product_color[wish$product_color == "winered"] <- "red"

wish$product_color[wish$product_color == "Pink"] <- "pink"
wish$product_color[wish$product_color == "lightpink"] <- "pink"
wish$product_color[wish$product_color == "dustypink"] <- "pink"
wish$product_color[wish$product_color == "pink & white"] <- "pink"
wish$product_color[wish$product_color == "pink & blue"] <- "pink"
wish$product_color[wish$product_color == "pink & black"] <- "pink"
wish$product_color[wish$product_color == "pink & grey"] <- "pink"

wish$product_color[wish$product_color == "orange & camouflage"] <- "orange"
wish$product_color[wish$product_color == "apricot"] <- "orange"
wish$product_color[wish$product_color == "orange-red"] <- "orange"

wish$product_color[wish$product_color == "fluorescentgreen"] <- "green"
wish$product_color[wish$product_color == "darkgreen"] <- "green"
wish$product_color[wish$product_color == "lightkhaki"] <- "green"
wish$product_color[wish$product_color == "applegreen"] <- "green"
wish$product_color[wish$product_color == "army green"] <- "green"
wish$product_color[wish$product_color == "lightgreen"] <- "green"
wish$product_color[wish$product_color == "light green"] <- "green"
wish$product_color[wish$product_color == "khaki"] <- "green"
wish$product_color[wish$product_color == "Army green"] <- "green"
wish$product_color[wish$product_color == "armygreen"] <- "green"
wish$product_color[wish$product_color == "army"] <- "green"
wish$product_color[wish$product_color == "mintgreen"] <- "green"

wish$product_color[wish$product_color == "offblack"] <- "black"
wish$product_color[wish$product_color == "Black"] <- "black"
wish$product_color[wish$product_color == "coolblack"] <- "black"
wish$product_color[wish$product_color == "black & green"] <- "black"
wish$product_color[wish$product_color == "black & white"] <- "black"
wish$product_color[wish$product_color == "black & yellow"] <- "black"
wish$product_color[wish$product_color == "black & stripe"] <- "black"
wish$product_color[wish$product_color == "black & blue"] <- "black"

wish$product_color[wish$product_color == "camel"] <- "beige"
wish$product_color[wish$product_color == "ivory"] <- "beige"
wish$product_color[wish$product_color == "tan"] <- "beige"
wish$product_color[wish$product_color == "nude"] <- "beige"

wish$product_color[wish$product_color == "coffee"] <- "brown"
wish$product_color[wish$product_color == "brown & yellow"] <- "brown"

wish$product_color[wish$product_color == "lightgrey"] <- "gray"
wish$product_color[wish$product_color == "grey"] <- "gray"
wish$product_color[wish$product_color == "gray & white"] <- "gray"
wish$product_color[wish$product_color == "lightgray"] <- "gray"

wish$product_color[wish$product_color == "lightyellow"] <- "yellow"

wish$product_color[wish$product_color == "violet"] <- "purple"
wish$product_color[wish$product_color == "lightpurple"] <- "purple"

wish$product_color[wish$product_color == "rainbow"] <- "multicolor"
wish$product_color[wish$product_color == "floral"] <- "multicolor"
wish$product_color[wish$product_color == "greysnakeskinprint"] <- "multicolor"
wish$product_color[wish$product_color == "whitefloral"] <- "multicolor"
wish$product_color[wish$product_color == "jasper"] <- "multicolor"
wish$product_color[wish$product_color == "leopardprint"] <- "multicolor"
wish$product_color[wish$product_color == "star"] <- "multicolor"
wish$product_color[wish$product_color == "leopard"] <- "multicolor"
wish$product_color[wish$product_color == "camouflage"] <- "multicolor"

wish$product_color[wish$product_color == "rosegold"] <- "metalen"
wish$product_color[wish$product_color == "gold"] <- "metalen"
wish$product_color[wish$product_color == "silver"] <- "metalen"

wish$product_color[wish$product_color == ""] <- "other"

wish <- subset(wish, select = -c(title, shipping_option_name, shipping_option_price, shipping_is_express, merchant_has_profile_picture, merchant_profile_picture))

# write cleaned data to new file
write.csv(wish, "../image-tagging/wish_cleaned.csv")

