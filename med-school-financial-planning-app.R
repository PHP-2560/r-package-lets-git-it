library(shiny)

ui <- fluidPage(
  titlePanel("Standard Normal Probabilities"), 
  h3("Input a z-score below to calculate its correponding p-value."),
  sidebarLayout(
    sidebarPanel(
      numericInput("z", "z-score", 0)), #define the input and assign it a default value of 0
    mainPanel(
      textOutput("p_value")) #define the output
  ) 
)

server <- function(input, output) {
  output$p_value <- reactive({pnorm(input$z)}) #calculate p-values using pnorm
}

shinyApp(ui = ui, server = server)