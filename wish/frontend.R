library(shiny)
library(httr)
# https://stackoverflow.com/questions/33526256/dynamically-display-images-from-upload-in-shiny-ui/33529261

#ui
ui <- fluidPage(
  navbarPage("Project Kait",
     tabPanel("Input", 
        sidebarLayout(
          sidebarPanel(
            h2(id="subtitle", "Give your information of the product"),
            numericInput("numberoftags", 
                         h3("Amount of tags"), 
                         value = 1),
            numericInput("price", 
                         h3("Price"), 
                         value = 1),
            numericInput("discount", 
                         h3("Discount"), 
                         value = 1),
            checkboxInput("adboost", "Using ad boosts", FALSE),
            checkboxInput("shippingexpress", "Are you shipping express?", FALSE),
            checkboxInput("badgeshipping", "Badge Shipping", FALSE),
            checkboxInput("badgelocalproduct", "Badge Local Product", FALSE),
            textInput("imageurl", "Upload your photo", value = "",),
            submitButton("Update View", icon("refresh")),
          ),
          mainPanel(
            h2(id="subtitle", "Informatie over de unit sold"),
            textOutput("numberoftags"),
            textOutput("price"),
            textOutput("discount"),
            verbatimTextOutput("adboost"),
            verbatimTextOutput("shippingexpress"),
            verbatimTextOutput("badgeshipping"),
            verbatimTextOutput("badgelocalproduct"),
            verbatimTextOutput("imageurl"),
            tableOutput('files'),
            uiOutput('images')
          )
        ),
     ),
     tabPanel("Information on the algorithm", 
        sidebarLayout(
         sidebarPanel(
           h2(id="subtitle", "Information Kait")
         ),
         mainPanel(
           h2(id="graph-title", "balbalbla")
         )
     ),)
  )
)
#server
server <- function(input, output) {
  ## server things
  output$numberoftags <- renderText({
    paste("Number of tags you have chosen: ", input$numberoftags)
  })
  
  output$price <- renderText({
    paste("Price of the product you have chosen: ", input$price)
  })
  
  output$discount <- renderText({
    paste("Discount of the product you have chosen: ", input$discount)
  })
  
  output$imageurl <- renderText({
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
      paste("Image tags: ", image_confidence)
      # in de predict gooien
    } else {
        NULL
      }
  })
  
  output$adboost <- renderText({ input$adboost })
  output$shippingexpress <- renderText({ input$shippingexpress })
  output$badgeshipping <- renderText({ input$badgeshipping })
  output$badgelocalproduct <- renderText({ input$badgelocalproduct })
  
  # prepare data
  wish_unitsold_df_scaled$wish.unit_sold_label <- wish_unitsold_df$wish.unit_sold_label
  
  # kNN
  library(DMwR)
  library(class)
  
  idxs <- sample(1:nrow(wish_unitsold_df_scaled), as.integer(0.7*nrow(wish_unitsold_df_scaled)))
  wish_unitsold_df_scaled.train <- wish_unitsold_df_scaled[idxs,]
  wish_unitsold_df_scaled.test <- wish_unitsold_df_scaled[-idxs,]
  
  as.factor(wish_unitsold_df_scaled$wish.unit_sold_label)
  cl <- as.factor(wish_unitsold_df_scaled.train$wish.units_sold_label)
  
  # prediction
  nn3 <- knn(train= wish_unitsold_df_scaled.train, test = wish_unitsold_df_scaled.test, cl= cl, k=3)
  nn5 <- knn(train= wish_unitsold_df_scaled.train, test = wish_unitsold_df_scaled.test, cl =cl, k=5)
  nn7 <- knn(train= wish_unitsold_df_scaled.train, test = wish_unitsold_df_scaled.test, cl = cl, k=7)
  nn10 <- knn(train= wish_unitsold_df_scaled.train, test = wish_unitsold_df_scaled.test, cl =cl, k=10)
  
  knn.predict()
  acc.3 <- 100 * sum(wish_unitsold_df_scaled.test$image_confidence == nn3)/NROW(wish_unitsold_df_scaled.test$image_confidence) # 99.778%
  acc.5 <- 100 * sum(wish_unitsold_df_scaled.test$image_confidence == nn5)/NROW(wish_unitsold_df_scaled.test$image_confidence) # 99.778%
  acc.7 <- 100 * sum(wish_unitsold_df_scaled.test$image_confidence == nn7)/NROW(wish_unitsold_df_scaled.test$image_confidence) # 99.5575%
  acc.10 <- 100 * sum(wish_unitsold_df_scaled.test$image_confidence == nn10)/NROW(wish_unitsold_df_scaled.test$image_confidence) # 99.5575%
}

shinyApp(ui= ui, server = server)

