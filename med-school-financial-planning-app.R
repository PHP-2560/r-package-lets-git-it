library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(ggpubr)

ui <- fluidPage(
  titlePanel("Medical School Financial Planning"),
    sidebarLayout(
      sidebarPanel(
        #input undergraduate federal debt, default is $0
        numericInput("undergrad_federal_debt", "Undergraduate Federal Debt", 0),
        #input undergraduate private debt, default is $0
        numericInput("undergrad_private_debt", "Undergraduate Private Debt", 0),
        #input medical school federal debt, default is $0
        numericInput("med_federal_debt", "Medical School Federal Debt", 0),
        #input medical school private debt, default is $0
        numericInput("med_private_debt", "Medical School Private Debt", 0),
        #input average interest rate, default is 7.6%
        numericInput("avg_interest_rate", "Average Interest Rate", 0.076),
        #select a specialty, default is internal medicine
        #selectInput("specialty", label = h5("Select a specialty"),
                    #choices = specialty_info$Specialty, selected = specialty_info$Specialty[6]),
        #input years of training
        numericInput("PGY_education", "Years of Training", specialty_info$`Years of Training`[6]),
        #input average attending salary
        numericInput("avg_attending_salary", "Average Attending Salary", specialty_info$`Annual Salary`[6]),
        #input average residency salary, default is $65,000
        numericInput("avg_residency_salary", "Average Residency Salary", 65000),
        #input residency tax rate, default is 25%
        numericInput("residency_tax", "Residency Tax Rate", 0.25),
        #input attending tax rate, default is 35%
        numericInput("attending_tax", "Attending Tax Rate", 0.35),
        #input growth rate, defualt is 5%
        numericInput("growth_rate", "Forgiveness Prep Fund Growth Rate", 0.05)
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

standard_frame <- data.frame()

server <- function(input, output, session) {
  source("specialty_res.R")
  source("specialty_salary.R")
  source("grow.R")
  source("pay.per.year.R")
  source("payments.R")
  source("income.driven.R")
  source("income.driven.vector.R")
  source("forgiveness.prep.fund_ten.R")
  observe({
    updateNumericInput(session, "undergrad_federal_debt", "Undergraduate Federal Debt", 0)
  })
  observe({
    updateNumericInput(session, "undergrad_private_debt", "Undergraduate Private Debt", 0)
  })
  observe({
    updateNumericInput(session, "med_federal_debt", "Medical School Federal Debt", 0)
  })
  observe({
    updateNumericInput(session, "med_private_debt", "Medical School Private Debt", 0)
  })
  observe({
    updateNumericInput(session, "avg_interest_rate", "Average Interest Rate", 0.076)
  })
  observe({
    updateSelectInput(session, "specialty", label = h5("Select a specialty"))
  })
  observe({
    updateNumericInput(session, "PGY_education", "Years of Training")
  })
  observe({
    updateNumericInput(session, "avg_attending_salary", "Average Attending Salary")
  })
  observe({
    updateNumericInput(session, "avg_residency_salary", "Average Residency Salary", 65000)
  })
  observe({
    updateNumericInput(session, "residency_tax", "Residency Tax Rate", 0.25)
  })
  observe({
    updateNumericInput(session, "attending_tax", "Attending Tax Rate", 0.35)
  })
  observe({
    updateNumericInput(session, "growth_rate", "Forgiveness Prep Fund Growth Rate", 0.05)
  })

  output$debt <- renderPlot({
    res <- input$PGY_education
    salary <- input$avg_attending_salary
    years <- c(1:20)
    gross <- vector(length = 20)
    for (i in 1:res) {
      gross[i] <- input$avg_residency_salary
    }
    for (k in (res+1):20) {
      gross[k] <- salary
    }
    debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
    
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
    
    #10-YEAR LOAN REPAYMENT OPTION
    private.total <- input$undergrad_private_debt + input$med_private_debt #need to make this a thing
    
    federal.total <- input$undergrad_federal_debt + input$med_federal_debt #need this value
    
    priv.payment <- pay.per.year(private.total, input$avg_interest_rate)
    
    stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {
      stand.repay[k] <- priv.payment
    }
    priv.remaining <- vector(length=20)
    for(i in 1:res) {
      priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)
    }
    for(k in (res+1):20) {
      priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
      priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)
    }
    
    #For the remaining federal debt they will be repaying 10% of their income for 20 years:
    #gross should have already been called
    percent.payment <- gross[1:10]*0.1
    percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0)
    forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
    prep.payment.amount <- forgiveness.prep.fund_ten(forgiven.amount, input$growth_rate, input$attending_tax, print.saved=0)
    prep.payment <- vector(length = 10)
    for(i in 1:res){
      prep.payment[i]=0 
    }
    for(k in (res+1):10){
      prep.payment[k]=prep.payment.amount
    }
    
    fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
    
    
    forgiveness.fund.annual_ten<- vector(length = 10)
    forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)
    } #*this would be the vector of how much you have saved to pay off the final forgiven amount
    
    #combine the two repayments (fed and private) below
    prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
    fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) #these three make the important vectors all go to 20 so they work nicely in the df
    
    payment_ten <- vector(length = 20)
    for(i in 1:20) {
      payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
    }
    payment_ten[10] <- payment_ten[10] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
    total_paid_ten <- cumsum(payment_ten) #cumulative payments
    disposable_ten <-  vector(length = 20)
    for (i in 1:res){
      disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]
    }
    for (k in (res+1):20){
      disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]
    }
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
    cum_disposable_ten <- cumsum(disposable_ten)
    
    debt_left_ten <- priv.remaining + fed.remaining 
    
    ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    p1 <- ggplot(standard_frame, aes(x = years)) +
      geom_line(aes(y = debt_left_standard, color = "Debt Remaining")) +
      scale_color_manual("",
                         breaks = c("Debt Remaining"),
                         values = c("Debt Remaining" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("Standard Repayment Option") + 
      theme_minimal()
    
    p2 <- ggplot(ten_year_frame, aes(x = years)) +
      geom_line(aes(y = debt_left_ten, color = "Debt Remaining")) +
      scale_color_manual("",
                         breaks = c("Debt Remaining"),
                         values = c("Debt Remaining" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("10-Year 10% Repayment Option") + 
      theme_minimal()
    
    ggarrange(p1, p2)
  })
    
  output$year_by_year <- renderPlot({
    res <- input$PGY_education
    salary <- input$avg_attending_salary
    years <- c(1:20)
    gross <- vector(length = 20)
    for (i in 1:res) {
      gross[i] <- input$avg_residency_salary
    }
    for (k in (res+1):20) {
      gross[k] <- salary
    }
    debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
    
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
    
    #10-YEAR LOAN REPAYMENT OPTION
    private.total <- input$undergrad_private_debt + input$med_private_debt #need to make this a thing
    
    federal.total <- input$undergrad_federal_debt + input$med_federal_debt #need this value
    
    priv.payment <- pay.per.year(private.total, input$avg_interest_rate)
    
    stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {
      stand.repay[k] <- priv.payment
    }
    priv.remaining <- vector(length=20)
    for(i in 1:res) {
      priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)
    }
    for(k in (res+1):20) {
      priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
      priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)
    }
    
    #For the remaining federal debt they will be repaying 10% of their income for 20 years:
    #gross should have already been called
    percent.payment <- gross[1:10]*0.1
    percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0)
    forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
    prep.payment.amount <- forgiveness.prep.fund_ten(forgiven.amount, input$growth_rate, input$attending_tax, print.saved=0)
    prep.payment <- vector(length = 10)
    for(i in 1:res){
      prep.payment[i]=0 
    }
    for(k in (res+1):10){
      prep.payment[k]=prep.payment.amount
    }
    
    fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
    
    
    forgiveness.fund.annual_ten<- vector(length = 10)
    forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)
    } #*this would be the vector of how much you have saved to pay off the final forgiven amount
    
    #combine the two repayments (fed and private) below
    prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
    fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) #these three make the important vectors all go to 20 so they work nicely in the df
    
    payment_ten <- vector(length = 20)
    for(i in 1:20) {
      payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
    }
    payment_ten[10] <- payment_ten[10] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
    total_paid_ten <- cumsum(payment_ten) #cumulative payments
    disposable_ten <-  vector(length = 20)
    for (i in 1:res){
      disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]
    }
    for (k in (res+1):20){
      disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]
    }
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
    cum_disposable_ten <- cumsum(disposable_ten)
    
    debt_left_ten <- priv.remaining + fed.remaining 
    
    ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    p1 <- ggplot(standard_frame, aes(x = years)) +
      geom_line(aes(y = gross, color = "Gross Income")) +
      geom_line(aes(y = disposable_standard, color = "Disposable Income")) +
      geom_line(aes(y = debt_payment_standard, color = "Debt Payment")) +
      scale_color_manual("",
                         breaks = c("Gross Income", "Disposable Income", "Debt Payment"),
                         values = c("Gross Income" = "green", "Disposable Income" = "blue", "Debt Payment" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("Standard Repayment Option") + 
      theme_minimal()
    
    p2 <- ggplot(ten_year_frame, aes(x = years)) +
      geom_line(aes(y = gross, color = "Gross Income")) +
      geom_line(aes(y = disposable_ten, color = "Disposable Income")) +
      geom_line(aes(y = payment_ten, color = "Debt Payment")) +
      scale_color_manual("",
                         breaks = c("Gross Income", "Disposable Income", "Debt Payment"),
                         values = c("Gross Income" = "green", "Disposable Income" = "blue", "Debt Payment" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("10-Year 10% Repayment Option") + 
      theme_minimal()
    
    ggarrange(p1, p2)
  })
  
  
  output$lifetime_earnings <- renderPlot({
    res <- input$PGY_education
    salary <- input$avg_attending_salary
    years <- c(1:20)
    gross <- vector(length = 20)
    for (i in 1:res) {
      gross[i] <- input$avg_residency_salary
    }
    for (k in (res+1):20) {
      gross[k] <- salary
    }

    debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
    
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
    
    #10-YEAR LOAN REPAYMENT OPTION
    private.total <- input$undergrad_private_debt + input$med_private_debt #need to make this a thing
    
    federal.total <- input$undergrad_federal_debt + input$med_federal_debt #need this value
    
    priv.payment <- pay.per.year(private.total, input$avg_interest_rate)
    
    stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {
      stand.repay[k] <- priv.payment
    }
    priv.remaining <- vector(length=20)
    for(i in 1:res) {
      priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)
    }
    for(k in (res+1):20) {
      priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
      priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)
    }
    
    #For the remaining federal debt they will be repaying 10% of their income for 20 years:
    #gross should have already been called
    percent.payment <- gross[1:10]*0.1
    percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0)
    forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
    prep.payment.amount <- forgiveness.prep.fund_ten(forgiven.amount, input$growth_rate, input$attending_tax, print.saved=0)
    prep.payment <- vector(length = 10)
    for(i in 1:res){
      prep.payment[i]=0 
    }
    for(k in (res+1):10){
      prep.payment[k]=prep.payment.amount
    }
    
    fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
    
    
    forgiveness.fund.annual_ten<- vector(length = 10)
    forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
      forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)
    } #*this would be the vector of how much you have saved to pay off the final forgiven amount
    
    #combine the two repayments (fed and private) below
    prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
    fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) #these three make the important vectors all go to 20 so they work nicely in the df
    
    payment_ten <- vector(length = 20)
    for(i in 1:20) {
      payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
    }
    payment_ten[10] <- payment_ten[10] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
    total_paid_ten <- cumsum(payment_ten) #cumulative payments
    disposable_ten <-  vector(length = 20)
    for (i in 1:res){
      disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]
    }
    for (k in (res+1):20){
      disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]
    }
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
    cum_disposable_ten <- cumsum(disposable_ten)
    
    debt_left_ten <- priv.remaining + fed.remaining 
    
    ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    p1 <- ggplot(standard_frame, aes(x = years)) +
      geom_line(aes(y = cum_disposable_standard, color = "Cumulative Disposable Income")) +
      geom_line(aes(y = total_paid_standard, color = "Total Debt Paid")) +
      scale_color_manual("",
                         breaks = c("Cumulative Disposable Income", "Total Debt Paid"),
                         values = c("Cumulative Disposable Income" = "green", "Total Debt Paid" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("Standard Repayment Option") + 
      theme_minimal()
    
    p2 <- ggplot(ten_year_frame, aes(x = years)) +
      geom_line(aes(y = cum_disposable_ten, color = "Cumulative Disposable Income")) +
      geom_line(aes(y = total_paid_ten, color = "Total Debt Paid")) +
      scale_color_manual("",
                         breaks = c("Cumulative Disposable Income", "Total Debt Paid"),
                         values = c("Cumulative Disposable Income" = "green", "Total Debt Paid" = "red")) +
      scale_y_continuous(labels = comma) +
      xlab("Years After Medical School Graduation") +
      ylab("Dollars") +
      ggtitle("10-Year 10% Repayment Option") +
      theme_minimal()
    
    ggarrange(p1, p2)
    
  })

}

shinyApp(ui = ui, server = server)