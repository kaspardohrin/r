library(shiny)
library(httr)
require(dplyr)

require(tidyverse)
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
            checkboxInput("urgencybanner", "Urgency banner", FALSE),
            checkboxInput("shippingexpress", "Are you shipping express?", FALSE),
            numericInput("merchantrating", 
                         h3("Merchant rating"), 
                         value = 1),
            checkboxInput("badgeshipping", "Badge Shipping", FALSE),
            checkboxInput("badgelocalproduct", "Badge Local Product", FALSE),
            checkboxInput("badgequalityproduct", "Badge Local Product", FALSE),
            #textInput("imageurl", "Upload your photo", value = "",),
            actionButton("submit", "submit"),
          ),
          mainPanel(
            h2(id="subtitle", "Informatie over de unit sold"),
            uiOutput('table'),
            #verbatimTextOutput("imageurl"),
            #tableOutput('files'),
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
  
  # prepare data
  # dataframe
  Data = reactive({
    print(input$submit)
    if (input$submit > 0) {
      predict_data =  data.frame(
        wish.unit_sold_label = "?",
        wish.price = input$price,
        wish.discount = input$discount,
        wish.numberof_tags_label = input$numberoftags,
        wish.uses_ad_boosts = input$adboost,
        wish.shipping_is_express = input$shippingexpress,
        wish.merchant_rating = input$merchantrating,
        wish.badge_fast_shipping = input$badgeshipping,
        wish.badge_local_product = input$badgelocalproduct,
        wish.badge_product_quality = input$badgequalityproduct,
        wish.has_urgency_banner = input$urgencybanner,
        image_confidence = input$numberoftags,
      ) 
      return(list(df=predict_data))
    }
  })
      
  output$table <- renderTable({
    if (is.null(Data())) {return()}
    print(Data()$df)
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
  output$unitssold <- renderText({
    if (is.null(Data())) {return()}
    
  })
}

shinyApp(ui= ui, server = server)

