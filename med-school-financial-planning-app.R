library(shiny)
library(ggplot2)
library(dplyr)

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

server <- function(input, output) {
  #x-axis of PGY years, y-axis of dollars
  #1 color is gross income (stop at 20 years)
  #1 color is disposale income (post-taxes and debt repayment)
  #1 color is debt repayment per year
  source("specialty_res.R")
  source("specialty_salary.R")
  source("grow.R")
  source("pay.per.year.R")
  source("payments.R")
  
  years <- c(1:20)
  gross <- vector(length = 20)
  for (i in 1:res) {
    gross[i] <- 65000
  }
  for (k in (res+1):20) {
    gross[k] <- salary
  }

  debt_total <- input$total_federal_debt + input$total_private_debt
  
  debt_payment <- pay.per.year(debt_total, input$avg_interest_rate)
  payments_output_standard <- payments(grow(debt_total, input$avg_interest_rate, n = res), input$avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
  for (i in 1:res) {
    debt_payment_standard[i] <- 0
  }
  for (k in (res+1):20) {
    debt_payment_standard[k] <- debt_payment
  }
  debt_payment_standard
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  
  debt_left_standard <- vector(length = 20)
  debt_left_standard[1] <- grow(debt_total, input$avg_interest_rate)
  for (i in 2:res) {
    debt_left_standard[i] <- grow(debt_left_standard[i-1], input$avg_interest_rate)
  }
  for (k in (res+1):20) {
    debt_left_standard[k] <- payments_output_standard[k-res]
  }
  debt_left_standard

  disposable_standard <- vector(length = 20)
  for (i in 1:res){
    disposable_standard[i] <- gross[i]*(1-input$residency_tax)-debt_payment_standard[i]
  }
  for (k in (res+1):20){
    disposable_standard[k] <- gross[k]*(1-input$attending_tax)-debt_payment_standard[k]
  }
  disposable_standard
  
  cum_disposable_standard <- cumsum(disposable_standard)
  
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
  
  output$year_by_year <- renderPlot({
    input$specialty
    input$PGY_education
    input$avg_residency_salary
    input$residency_tax
    input$attending_tax
    
  })
  output$lifetime_earnings
  output$debt <- renderPlot({
    ggplot(standard_frame, aes(x = years, y = debt_left_standard)) + geom_smooth() + geom_point()
  })
}

shinyApp(ui = ui, server = server)