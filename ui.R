library(shiny)

ui <- fluidPage(
  headerPanel('Text Prediction Application'),
  
  sidebarPanel(
  textInput(inputId = "text", label = "Text", placeholder = "Enter text", value = ""),
  htmlOutput('out_text'),
  tags$head(
    tags$script(src="text.js")
  )
  )


)

