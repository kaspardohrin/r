library("shiny") 
library(httr)
require(dplyr)
require(tidyverse)
require(jsonlite)
library("class")
library("imputeTS")

wish_df <- read.csv("/Users/ireneprins/wish_unitssold_df.csv")

wish_df$wish.has_urgency_banner <- na.replace(wish_df$wish.has_urgency_banner, 0)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Team Kait - predict your units sold")
    ,
    sidebarPanel(
      wellPanel(
        textInput("numberoftags", "Number of tags", ""),
        textInput("retailprice", "Retail Price", ""),
        textInput('newprice', "New Price",""),
        textInput('merchantrating', "Merchant Rating",""),
        checkboxInput("adboost", "Using ad boosts", FALSE),
        checkboxInput("urgencybanner", "Urgency banner", FALSE),
        checkboxInput("shippingexpress", "Shipping express", FALSE),
        checkboxInput("badgeshipping", "Badge Shipping", FALSE),
        checkboxInput("badgelocalproduct", "Badge Local Product", FALSE),
        checkboxInput("badgequalityproduct", "Badge Quality Product", FALSE),
        textInput("imageurl", "Upload your photo", value = "",),
        actionButton("submit","Submit")
      )
    ),
    mainPanel(
      h3(id="subtitle", "Voorspelling"),
      textOutput("unitssold"),
      h3(id="subtitle", "Theorie"),
      p("Voor deze voorspelling gebruiken we het algoritme knn met knn = 3. Het 
        model hebben we getrains met de wish dataset zie: https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish.
        Units sold worden voorspeld door middel van categorieen. Er zijn vier categorieen: 1, 2, 3 en 4. 
        Waarin 1 laag aantal unitssold is en 4 hoog aantal unitssold"),
      )
  ))

server <- function(input, output) {
  image_confidence = reactive({
    if(nchar(input$imageurl) > 0) {
      api_key = 'acc_fcf864a5cff4e89'
      api_secret = '00b66a47bb5c85571308b727bdf9e032'
      imgURL <- input$imageurl
      
      # https://shiny.rstudio.com/images/reactivity_diagrams/faithful.png
      result <- GET(url=sprintf("https://api.imagga.com/v2/tags?image_url=%s", imgURL), authenticate(api_key, api_secret), treshold="30.0")
      tags <- content(result, "text")
      
      # data frame van maken
      currentTaglist <- fromJSON(tags)
      image_confidence = mean(currentTaglist$result$tags$confidence[1:10])
      
      # normaliseren van gemiddelde
      image_confidence <- image_confidence * 0.01
      print(image_confidence)
      return(image_confidence)
    }
  })
  
  Data = reactive({
    if (input$submit > 0) {
      df <- data.frame(
        unitssoldlabel = as.integer(1),
        numberoftags=as.integer(input$numberoftags),
        retailprice=as.integer(input$retailprice),
        newprice=as.integer(input$newprice),
        merchantrating=as.integer(input$merchantrating),
        adboost= as.integer(as.logical(input$adboost)),
        urgencybanner=  as.integer(as.logical(input$urgencybanner)),
        shippingexpress= as.integer(as.logical(input$shippingexpress)),
        badgeshipping= as.integer(as.logical(input$badgeshipping)),
        badgelocalproduct= as.integer(as.logical(input$badgelocalproduct)),
        badgequalityproduct= as.integer(as.logical(input$badgequalityproduct)),
        image_confidence = as.double(image_confidence())
      )
      return(df)
    }
  })
  
  #https://stackoverflow.com/questions/19706320/create-a-data-frame-using-text-input-in-shiny
  output$unitssold <- renderText({
    if (is.null(Data())) {return()}
    wish_df[nrow(wish_df) + 1,] <- Data()
    
    #scale dataframe
    wish_df_scaled <- data.frame(scale(wish_df))
    
    wish_df_scaled.train <- wish_df_scaled[1:(nrow(wish_df_scaled)-1),]
    wish_df_scaled.test <- tail(wish_df_scaled,1)
    
    print(wish_df_scaled.test)
    cl <- factor(wish_df_scaled.train$wish.unit_sold_label)

    nn3 <- knn(train= wish_df_scaled.train, test = wish_df_scaled.test, cl= cl, k=3)
    
    print(nn3)
    if(nn3 == -1.81812235432563) {
      print("Your predicted unitssold are category 1")
      paste0("Your predicted unitssold are category 1")
    } else if (nn3 == -0.810961050130856){
      print("Your predicted unitssold are category 2")
      paste0("Your predicted unitssold are category 2")
    } else if (nn3 == 0.196200254063917){
      print("Your predicted unitssold are category 3")
      paste0("Your predicted unitssold are category 3")
    } else if (nn3 == 1.20336155825869){
      print("Your predicted unitssold are category 4")
      paste0("Your predicted unitssold are category 4")
    } else {
      print("er ging iets mis")
    }
      
  })
  
}

shinyApp(ui= ui, server = server)