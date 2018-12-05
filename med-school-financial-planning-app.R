library(shiny)

ui <- fluidPage(
  titlePanel("Medical School Financial Planning"),
    sidebarLayout(
      sidebarPanel(
        #input total federal debt, default is $0
        numericInput("total_federal_debt", "Total Federal Debt", 0),
        #input total private debt, default is $0
        numericInput("total_private_debt", "Total Private Debt", 0),
        #input average interest rate, default is 7.6%
        numericInput("avg_interest_rate", "Average Interest Rate", 0.076),
        #select a specialty, default is internal medicine
        selectInput("specialty", label = h5("Select a specialty"),
                    choices = specialty_info$Specialty, selected = specialty_info$Specialty[6]),
        #input years of training
        numericInput("PGY_education", "Years of Training", specialty_info$`Years of Training`[6]),
        #input average residency salary, default is $65,000
        numericInput("avg_residency_salary", "Average Residency Salary", 65000),
        #input residency tax rate, default is 25%
        numericInput("residency_tax", "Residency Tax Rate", 0.25),
        #input attending tax rate, default is 35%
        numericInput("attending_tax", "Attending Tax Rate", 0.35)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Year-by-year", plot("year_by_year")),
          tabPanel("Lifetime Earnings", plot("lifetime_earnings")),
          tabPanel("Debt Repayment", plot("debt"))
          
        )
      )
    ),

  
  titlePanel("Medical School Financial Planning"), 
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