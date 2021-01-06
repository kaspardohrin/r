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
            tags$style(HTML("#subtitle{color: black;font-size:18px;margin-top:0;margin-bottom:15px;}",
                            "#graph-title{color: black;font-size:18px;margin-top:0;margin-bottom:15px;}",
            )),
            numericInput("numberoftags", 
                         h3("Amount of tags"), 
                         value = 1),
            fileInput(inputId = 'files', 
                      label = 'Select an Image',
                      multiple = TRUE,
                      accept=c('image/png', 'image/jpeg'))
          ),
          mainPanel(
            h2(id="subtitle", "Informatie over de unit sold"),
            textOutput("numberoftags"),
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
  
  output$files <- renderTable(input$files)
  
  # pas aan voor local storage of file
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    file.copy(files$datapath, "/Users/ireneprins")
    files
  })
  
  
  output$images <- renderUI({
    if(is.null(input$files)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(files()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    if(is.null(input$files)) return(NULL)
    for (i in 1:nrow(files()))
    {
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        output[[imagename]] <- 
          renderImage({
            list(src = files()$datapath[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
}

shinyApp(ui= ui, server = server)

