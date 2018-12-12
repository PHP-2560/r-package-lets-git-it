library(shiny)
library(ggplot2)

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
          tabPanel("Year-by-year", plotOutput("year_by_year")),
          tabPanel("Lifetime Earnings", plotOutput("lifetime_earnings")),
          tabPanel("Debt Repayment", plotOutput("debt"))
        )
      )
    )
  ) 

library(dplyr)

specialty_res <- function(specialty) {
  res <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Years of Training`)
  print(as.numeric(res))
}

res <- specialty_res("Plastic Surgery")

specialty_salary <- function(specialty) {
  salary <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Annual Salary`)
  print(as.numeric(salary))
}

salary <- specialty_salary("Plastic Surgery")



server <- function(input, output) {
  #x-axis of PGY years, y-axis of dollars
  #1 color is gross income (stop at 20 years)
  #1 color is disposale income (post-taxes and debt repayment)
  #1 color is debt repayment per year
  output$year_by_year <- renderPlot({
    input$specialty
    input$PGY_education
    input$avg_residency_salary
    input$residency_tax
    input$attending_tax
    
  })
  output$lifetime_earnings
  output$debt
}

shinyApp(ui = ui, server = server)