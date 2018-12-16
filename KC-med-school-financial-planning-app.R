library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(ggpubr)
library(readr)
library(scales)
library(ggrepel)

ui <- fluidPage(
  titlePanel("Medical School Financial Planning"),
    sidebarLayout(
      sidebarPanel(
        #input undergraduate federal debt, default is $0
        numericInput("undergrad_federal_debt", "Undergraduate Federal Debt (at Medical School Graduation)", 0),
        #input undergraduate private debt, default is $0
        numericInput("undergrad_private_debt", "Undergraduate Private Debt (at Medical School Graduation)", 0),
        #input medical school federal debt, default is $0
        numericInput("med_federal_debt", "Medical School Federal Debt (at Medical School Graduation)", 0),
        #input medical school private debt, default is $0
        numericInput("med_private_debt", "Medical School Private Deb (at Medical School Graduation)", 0),
        #input average interest rate, default is 7.6%
        numericInput("avg_interest_rate", "Average Interest Rate on Loans", 0.076),
        #input years of training
        numericInput("PGY_education", "Years of Training (Residency+Fellowship)", 3),
        #input average attending salary
        numericInput("avg_attending_salary", "Average Attending Salary", 230000),
        #input average residency salary, default is $65,000
        numericInput("avg_residency_salary", "Average Residency Salary", 65000),
        #input residency tax rate, default is 25%
        numericInput("residency_tax", "Residency Tax Rate", 0.25),
        #input attending tax rate, default is 35%
        numericInput("attending_tax", "Attending Tax Rate", 0.35),
        #input growth rate, defualt is 5%
        numericInput("growth_rate", "Forgiveness Prep Fund Growth Rate", 0.05),
          helpText("At the end of the 10- and 20-year 10% repayment plan, the 'forgiven' loan amount is taxable. To ultimately pay this off, you need to put money away each year. This is the expected rate at which your savings fund will grow.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Info on Specialties", plotOutput("specials")),
          tabPanel("Year-by-year", plotOutput("year_by_year")),
          tabPanel("Lifetime Earnings", plotOutput("lifetime_earnings")),
          tabPanel("Debt Repayment", plotOutput("debt")),
          tabPanel("Description", htmlOutput("Repayments"))
        )
      )
    )
  ) 

server <- function(input, output, session) {
  source("specialty_res.R")
  source("specialty_salary.R")
  source("grow.R")
  source("payments.R")
  source("pay.per.year.R")
  source("income.driven.R")
  source("income.driven.vector.R")
  source("forgiveness.prep.fund.R")
  observe({updateNumericInput(session, "undergrad_federal_debt", "Undergraduate Federal Debt (at Medical School Graduation)", 0)})
  observe({updateNumericInput(session, "undergrad_private_debt", "Undergraduate Private Debt (at Medical School Graduation)", 0)})
  observe({updateNumericInput(session, "med_federal_debt", "Medical School Federal Debt (at Medical School Graduation)", 0)})
  observe({updateNumericInput(session, "med_private_debt", "Medical School Private Deb (at Medical School Graduation)", 0)})
  observe({updateNumericInput(session, "avg_interest_rate", "Average Interest Rate on Loans", 0.076)})
  observe({updateNumericInput(session, "PGY_education", "Years of Training (Residency+Fellowship)")})
  observe({updateNumericInput(session, "avg_attending_salary", "Average Attending Salary")})
  observe({updateNumericInput(session, "avg_residency_salary", "Average Residency Salary", 65000)})
  observe({updateNumericInput(session, "residency_tax", "Residency Tax Rate", 0.25)})
  observe({updateNumericInput(session, "attending_tax", "Attending Tax Rate", 0.35)})
  observe({updateNumericInput(session, "growth_rate", "Forgiveness Prep Fund Growth Rate", 0.05)})

  
  
output$specials <- renderPlot({
  
#will need to make it renderPlot and plotOutput to make it show the graph
  Specialty <- c("Anesthesiology", "Cardiology", "Dermatology", "Emergency Medicine", "Endocrinology", "Family Practice", "Gastroenterology", "General Surgery", "Immunology", "Infectious Disease", "Internal Medicine", "Nephrology", "Neurology", "Neurosurgery", "Obstetrics/Gynecology", "Oncology", "Ophthalmology", "Orthopedic Surgery", "Otolaryngology", "Pathology", "Pediatrics", "Physical Medicine", "Plastic Surgery", "Psychiatry", "Pulmonary", "Radiation Oncology", "Radiology, Diagnostic", "Rheumatology", "Urology")
  Training <- c(4, 6, 4, 3, 5, 3, 6, 5, 5, 5, 3, 5, 4, 7, 4, 5, 4, 5, 5, 4, 3, 4, 6, 4, 5, 5, 5, 5, 5)
  Earnings <- c(386000, 423000, 392000, 350000, 212000, 219000, 408000, 322000, 272000, 231000, 230000, 294000, 244000, 663000, 300000, 363000, 357000, 497000, 383000, 286000, 212000, 269000, 501000, 273000, 321000, 468000, 401000, 257000, 373000)
  specialty_info <- data.frame(Specialty, Training, Earnings)


  plotme <- ggplot(specialty_info, aes(Training, Earnings)) +
    geom_smooth(method = "lm") +
    geom_point() +
    scale_y_continuous(labels = comma) +
    xlab("Years of Training") +
    ylab("Average Annual Salary (in USD)") +
    ggtitle("Average Annual Earnings for Each Specialty")+
    theme_economist() + scale_color_economist() +
    geom_label_repel(aes(label = Specialty), box.padding   = 0.35,
                     point.padding = 0.5,
                     segment.color = 'grey50',
                    size = 2)
  
  
  ggarrange(plotme)
})
  
  
  
output$debt <- renderPlot({
#First, we will initialize values that are used throughout (ex. salary, debt, interest rate, residency lenghth, etc. This occurs immediately below)
  res <- input$PGY_education
  salary <- input$avg_attending_salary
  in.military <- c(87965, 99726.5, 115671.5, 128496.5, 136504, 141026, 147916.5, 153001, 156988.5, 163579, 168533.5, 176593, 180956, 183542, 187181.5, 198565, 199579.5, 206582, 213525.5, 206792.5) #filling in the military salary expected for the first 20 years as a military doc
  military.gross <- vector(length=20) #the below if statement is due to military policy. 5 years or less of residency and you owe them 4 years, anything past 5 and you owe them the length of the residency less one year
    if(res<=5) {for(i in 1:(res+4)) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+5):20) {military.gross[j] <- input$avg_attending_salary} }#civilian physician salary
    if(res>5) {for(i in 1:(res+(res-1))) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+res):20) {military.gross[j] <- input$avg_attending_salary}} #civilian physician salary
  years <- c(1:20)
  gross <- vector(length = 20)
    for (i in 1:res) {gross[i] <- input$avg_residency_salary}
    for (k in (res+1):20) {gross[k] <- salary}
  debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
  private.total <- input$undergrad_private_debt + input$med_private_debt
  federal.total <- input$undergrad_federal_debt + input$med_federal_debt 
    
    
#STANDARD: Now we begin to get the values for the standard output
  debt_payment <- pay.per.year(debt_total, input$avg_interest_rate, res, years = 20)
  payments_output_standard <- payments(grow(debt_total, input$avg_interest_rate, n = res), input$avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
    for (i in 1:res) {debt_payment_standard[i] <- 0}
    for (k in (res+1):20) {debt_payment_standard[k] <- debt_payment}
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  debt_left_standard <- vector(length = 20)
    debt_left_standard[1] <- grow(debt_total, input$avg_interest_rate)
    for (i in 2:res) {debt_left_standard[i] <- grow(debt_left_standard[i-1], input$avg_interest_rate)}
    for (k in (res+1):20) {debt_left_standard[k] <- payments_output_standard[k-res]}
  disposable_standard <- vector(length = 20)
    for (i in 1:res){disposable_standard[i] <- gross[i]*(1-input$residency_tax)-debt_payment_standard[i]}
    for (k in (res+1):20){disposable_standard[k] <- gross[k]*(1-input$attending_tax)-debt_payment_standard[k]}
  cum_disposable_standard <- cumsum(disposable_standard)
    
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
    
    
#10-YEAR LOAN REPAYMENT OPTION- values calculated below, first the private payments then the federal ones
  #We will be modeling paying the private loans off over 20 years after medschool (but waiting until after residency to begin paying)
  priv.payment <- pay.per.year(private.total, input$avg_interest_rate, res, years = 20)
  stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {stand.repay[k] <- priv.payment}
  priv.remaining <- vector(length=20)
    for(i in 1:res) {priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
    priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)}
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross[1:10]*0.1 #this is the annual payment (10% of income)
    percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0) #it needs to be a 20-year vector, but they will stop paying at year 5
  forgiven.amount.ten <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount.ten, input$growth_rate, input$attending_tax, res, years = 10, print.saved=0)
  prep.payment <- vector(length = 10)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):10){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_ten<- vector(length = 10)
    forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
    forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  #combining the two above repayments (fed and private) below
  prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
  fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
    forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) 
  payment_ten <- vector(length = 20)
    for(i in 1:20) {payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_ten[10] <- payment_ten[10] + forgiven.amount.ten*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_ten <- cumsum(payment_ten) #cumulative payments
  disposable_ten <-  vector(length = 20)
    for (i in 1:res){disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]}
    for (k in (res+1):20){disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]}
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount.ten*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_ten <- cumsum(disposable_ten)
  debt_left_ten <- priv.remaining + fed.remaining 
    
  ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    
#20 YEAR REPAYMENT OPTION
  #Just as with the 10-year repayment, we will need to handle the private (not forgivable) loans first
  #the code for this is already calculated above
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross*0.1
  forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, input$growth_rate, input$attending_tax, res, print.saved=0)
  prep.payment <- vector(length = 20)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):20){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_twenty<- vector(length = 20)
    forgiveness.fund.annual_twenty[1:res] <- 0
    for(i in (res+1):20) {forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
    forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  #combine the two repayments (fed and private) below
  payment_twenty <- vector(length = 20)
    for(i in 1:20) {payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_twenty[20] <- payment_twenty[20] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
  disposable_twenty <-  vector(length = 20)
    for (i in 1:res){disposable_twenty[i] <- gross[i]*(1-input$residency_tax)-payment_twenty[i]}
    for (k in (res+1):19){disposable_twenty[k] <- gross[k]*(1-input$attending_tax)-payment_twenty[k]}
    disposable_twenty[20] <- gross[20]*(1-input$attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_twenty <- cumsum(disposable_twenty)
  debt_left_twenty <- priv.remaining + fed.remaining 
    
  twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)
    
    
#UNDERSERVED REPAYMENT OPTION
  total.debt.underserved <- input$undergrad_private_debt + input$undergrad_federal_debt 
  underserved.payment <- pay.per.year(total.debt.underserved, input$avg_interest_rate, res, years = 20)
  underserved.repay <- vector(length=20)
    for(i in 1:res) {underserved.repay[i]=0}
    for(k in (res+1):20) {underserved.repay[k] <- underserved.payment}
  underserved.remaining <- vector(length=20)
    for(i in 1:res) {underserved.remaining[i] <- grow(total.debt.underserved, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
    underserved.remaining[k] <- underserved.remaining[k]*(1+input$avg_interest_rate)}
  disposable_underserved <-  vector(length = 20)
    for (i in 1:res){disposable_underserved[i] <- gross[i]*(1-input$residency_tax)}
    for (k in (res+1):20){disposable_underserved[k] <- gross[k]*(1-input$attending_tax)-underserved.repay[k]}
  cum_disposable_underserved <- cumsum(disposable_underserved)
  total_paid_underserved <- cumsum(underserved.repay)
    
  underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
    
    
#MILITARY REPAYMENT OPTION
  total.debt.military <- input$undergrad_private_debt + input$undergrad_federal_debt 
  military.payment <- pay.per.year(total.debt.military, input$avg_interest_rate, res, years = 20)
  military.repay <- vector(length=20)
    for(i in 1:res) {military.repay[i]=0}
    for(k in (res+1):20) {military.repay[k] <- military.payment}
  military.remaining <- vector(length=20)
    for(i in 1:res) {military.remaining[i] <- grow(total.debt.military, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {military.remaining[k] <- (military.remaining[k-1] - military.payment)
    military.remaining[k] <- military.remaining[k]*(1+input$avg_interest_rate)}
  disposable_military <-  vector(length = 20)
    for (i in 1:res){disposable_military[i] <- military.gross[i]*(1-input$residency_tax)}
    disposable_military[1] <- disposable_military[1]+50000 #need to add 50k for signing bonus and UG salary
    for (k in (res+1):20){disposable_military[k] <- military.gross[k]*(1-input$attending_tax)-military.repay[k]}
  cum_disposable_military <- cumsum(disposable_military)
  total_paid_military <- cumsum(military.repay)
    
  military_frame <- data.frame(years, military.gross, disposable_military, cum_disposable_military, military.remaining, military.repay, total_paid_military)
    
    
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
    
  p3 <- ggplot(twenty_year_frame, aes(x = years)) +
      geom_line(aes(y = debt_left_twenty, color = "Debt Remaining")) +
      scale_color_manual("",
                         breaks = c("Debt Remaining"),
                         values = c("Debt Remaining" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("20-Year 10% Repayment Option") + 
        theme_minimal()
    
  p4 <- ggplot(underserved_frame, aes(x = years)) +
      geom_line(aes(y = underserved.remaining, color = "Debt Remaining")) +
      scale_color_manual("",
                         breaks = c("Debt Remaining"),
                         values = c("Debt Remaining" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("NHSC (Underserved) Repayment Option") + 
        theme_minimal()
    
  p5 <- ggplot(military_frame, aes(x = years)) +
      geom_line(aes(y = military.remaining, color = "Debt Remaining")) +
      scale_color_manual("",
                       breaks = c("Debt Remaining"),
                       values = c("Debt Remaining" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("Military Repayment Option") + 
        theme_minimal()
    
    
ggarrange(p1, p2, p3, p4, p5)
  })
    
  
  
  
output$year_by_year <- renderPlot({
#First, we will initialize values that are used throughout (ex. salary, debt, interest rate, residency lenghth, etc. This occurs immediately below)
  res <- input$PGY_education
  salary <- input$avg_attending_salary
  in.military <- c(87965, 99726.5, 115671.5, 128496.5, 136504, 141026, 147916.5, 153001, 156988.5, 163579, 168533.5, 176593, 180956, 183542, 187181.5, 198565, 199579.5, 206582, 213525.5, 206792.5) #filling in the military salary expected for the first 20 years as a military doc
  military.gross <- vector(length=20) #the below if statement is due to military policy. 5 years or less of residency and you owe them 4 years, anything past 5 and you owe them the length of the residency less one year
    if(res<=5) {for(i in 1:(res+4)) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+5):20) {military.gross[j] <- input$avg_attending_salary} }#civilian physician salary
    if(res>5) {for(i in 1:(res+(res-1))) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+res):20) {military.gross[j] <- input$avg_attending_salary}} #civilian physician salary
  years <- c(1:20)
  gross <- vector(length = 20)
    for (i in 1:res) {gross[i] <- input$avg_residency_salary}
    for (k in (res+1):20) {gross[k] <- salary}
  debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
  private.total <- input$undergrad_private_debt + input$med_private_debt
  federal.total <- input$undergrad_federal_debt + input$med_federal_debt 


#STANDARD: Now we begin to get the values for the standard output
  debt_payment <- pay.per.year(debt_total, input$avg_interest_rate, res, years = 20)
  payments_output_standard <- payments(grow(debt_total, input$avg_interest_rate, n = res), input$avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
    for (i in 1:res) {debt_payment_standard[i] <- 0}
    for (k in (res+1):20) {debt_payment_standard[k] <- debt_payment}
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  debt_left_standard <- vector(length = 20)
  debt_left_standard[1] <- grow(debt_total, input$avg_interest_rate)
    for (i in 2:res) {debt_left_standard[i] <- grow(debt_left_standard[i-1], input$avg_interest_rate)}
    for (k in (res+1):20) {debt_left_standard[k] <- payments_output_standard[k-res]}
  disposable_standard <- vector(length = 20)
    for (i in 1:res){disposable_standard[i] <- gross[i]*(1-input$residency_tax)-debt_payment_standard[i]}
    for (k in (res+1):20){disposable_standard[k] <- gross[k]*(1-input$attending_tax)-debt_payment_standard[k]}
  cum_disposable_standard <- cumsum(disposable_standard)
    
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
    
    
#10-YEAR LOAN REPAYMENT OPTION- values calculated below, first the private payments then the federal ones
  #We will be modeling paying the private loans off over 20 years after medschool (but waiting until after residency to begin paying)
  priv.payment <- pay.per.year(private.total, input$avg_interest_rate, res, years = 20)
  stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {stand.repay[k] <- priv.payment}
  priv.remaining <- vector(length=20)
    for(i in 1:res) {priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
    priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)}
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross[1:10]*0.1 #this is the annual payment (10% of income)
    percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0) #it needs to be a 20-year vector, but they will stop paying at year 5
  forgiven.amount.ten <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount.ten, input$growth_rate, input$attending_tax, res, years = 10, print.saved=0)
  prep.payment <- vector(length = 10)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):10){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_ten<- vector(length = 10)
    forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
    forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  #combining the two above repayments (fed and private) below
  prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
  fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) 
  payment_ten <- vector(length = 20)
    for(i in 1:20) {payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_ten[10] <- payment_ten[10] + forgiven.amount.ten*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_ten <- cumsum(payment_ten) #cumulative payments
  disposable_ten <-  vector(length = 20)
    for (i in 1:res){disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]}
    for (k in (res+1):20){disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]}
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount.ten*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_ten <- cumsum(disposable_ten)
  debt_left_ten <- priv.remaining + fed.remaining 
    
  ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    
#20 YEAR REPAYMENT OPTION
  #Just as with the 10-year repayment, we will need to handle the private (not forgivable) loans first
  #the code for this is already calculated above
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross*0.1
  forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, input$growth_rate, input$attending_tax, res, print.saved=0)
  prep.payment <- vector(length = 20)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):20){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_twenty<- vector(length = 20)
    forgiveness.fund.annual_twenty[1:res] <- 0
    for(i in (res+1):20) {forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
    forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  #combine the two repayments (fed and private) below
  payment_twenty <- vector(length = 20)
    for(i in 1:20) {payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_twenty[20] <- payment_twenty[20] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
  disposable_twenty <-  vector(length = 20)
    for (i in 1:res){disposable_twenty[i] <- gross[i]*(1-input$residency_tax)-payment_twenty[i]}
    for (k in (res+1):19){disposable_twenty[k] <- gross[k]*(1-input$attending_tax)-payment_twenty[k]}
    disposable_twenty[20] <- gross[20]*(1-input$attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_twenty <- cumsum(disposable_twenty)
  debt_left_twenty <- priv.remaining + fed.remaining 
    
  twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)
    
    
#UNDERSERVED REPAYMENT OPTION
  total.debt.underserved <- input$undergrad_private_debt + input$undergrad_federal_debt 
  underserved.payment <- pay.per.year(total.debt.underserved, input$avg_interest_rate, res, years = 20)
  underserved.repay <- vector(length=20)
    for(i in 1:res) {underserved.repay[i]=0}
    for(k in (res+1):20) {underserved.repay[k] <- underserved.payment}
  underserved.remaining <- vector(length=20)
    for(i in 1:res) {underserved.remaining[i] <- grow(total.debt.underserved, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
    underserved.remaining[k] <- underserved.remaining[k]*(1+input$avg_interest_rate)}
  disposable_underserved <-  vector(length = 20)
    for (i in 1:res){disposable_underserved[i] <- gross[i]*(1-input$residency_tax)}
    for (k in (res+1):20){disposable_underserved[k] <- gross[k]*(1-input$attending_tax)-underserved.repay[k]}
  cum_disposable_underserved <- cumsum(disposable_underserved)
  total_paid_underserved <- cumsum(underserved.repay)
    
  underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
    
    
#MILITARY REPAYMENT OPTION
  total.debt.military <- input$undergrad_private_debt + input$undergrad_federal_debt 
  military.payment <- pay.per.year(total.debt.military, input$avg_interest_rate, res, years = 20)
  military.repay <- vector(length=20)
    for(i in 1:res) {military.repay[i]=0}
    for(k in (res+1):20) {military.repay[k] <- military.payment}
  military.remaining <- vector(length=20)
    for(i in 1:res) {military.remaining[i] <- grow(total.debt.military, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {military.remaining[k] <- (military.remaining[k-1] - military.payment)
    military.remaining[k] <- military.remaining[k]*(1+input$avg_interest_rate)}
  disposable_military <-  vector(length = 20)
    for (i in 1:res){disposable_military[i] <- military.gross[i]*(1-input$residency_tax)}
  disposable_military[1] <- disposable_military[1]+50000 #need to add 50k for signing bonus and UG salary
    for (k in (res+1):20){disposable_military[k] <- military.gross[k]*(1-input$attending_tax)-military.repay[k]}
  cum_disposable_military <- cumsum(disposable_military)
  total_paid_military <- cumsum(military.repay)
    
  military_frame <- data.frame(years, military.gross, disposable_military, cum_disposable_military, military.remaining, military.repay, total_paid_military)
    
  
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
    
    p3 <- ggplot(twenty_year_frame, aes(x = years)) +
      geom_line(aes(y = gross, color = "Gross Income")) +
      geom_line(aes(y = disposable_twenty, color = "Disposable Income")) +
      geom_line(aes(y = payment_twenty, color = "Debt Payment")) +
      scale_color_manual("",
                         breaks = c("Gross Income", "Disposable Income", "Debt Payment"),
                         values = c("Gross Income" = "green", "Disposable Income" = "blue", "Debt Payment" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("20-Year 10% Repayment Option") + 
        theme_minimal()
    
    p4 <- ggplot(underserved_frame, aes(x = years)) +
      geom_line(aes(y = gross, color = "Gross Income")) +
      geom_line(aes(y = disposable_underserved, color = "Disposable Income")) +
      geom_line(aes(y = underserved.repay, color = "Debt Payment")) +
      scale_color_manual("",
                         breaks = c("Gross Income", "Disposable Income", "Debt Payment"),
                         values = c("Gross Income" = "green", "Disposable Income" = "blue", "Debt Payment" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("NHSC (Underserved) Repayment Option") + 
        theme_minimal()
    
    p5 <- ggplot(military_frame, aes(x = years)) +
      geom_line(aes(y = military.gross, color = "Gross Income")) +
      geom_line(aes(y = disposable_military, color = "Disposable Income")) +
      geom_line(aes(y = military.repay, color = "Debt Payment")) +
      scale_color_manual("",
                         breaks = c("Gross Income", "Disposable Income", "Debt Payment"),
                         values = c("Gross Income" = "green", "Disposable Income" = "blue", "Debt Payment" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("Military Repayment Option") + 
        theme_minimal()
    
    ggarrange(p1, p2, p3, p4, p5)
  })
  
  

  
output$lifetime_earnings <- renderPlot({
#First, we will initialize values that are used throughout (ex. salary, debt, interest rate, residency lenghth, etc. This occurs immediately below)
  res <- input$PGY_education
  salary <- input$avg_attending_salary
  in.military <- c(87965, 99726.5, 115671.5, 128496.5, 136504, 141026, 147916.5, 153001, 156988.5, 163579, 168533.5, 176593, 180956, 183542, 187181.5, 198565, 199579.5, 206582, 213525.5, 206792.5) #filling in the military salary expected for the first 20 years as a military doc
  military.gross <- vector(length=20) #the below if statement is due to military policy. 5 years or less of residency and you owe them 4 years, anything past 5 and you owe them the length of the residency less one year
    if(res<=5) {for(i in 1:(res+4)) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+5):20) {military.gross[j] <- input$avg_attending_salary} }#civilian physician salary
    if(res>5) {for(i in 1:(res+(res-1))) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+res):20) {military.gross[j] <- input$avg_attending_salary}} #civilian physician salary
  years <- c(1:20)
  gross <- vector(length = 20)
    for (i in 1:res) {gross[i] <- input$avg_residency_salary}
    for (k in (res+1):20) {gross[k] <- salary}
  debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
  private.total <- input$undergrad_private_debt + input$med_private_debt
  federal.total <- input$undergrad_federal_debt + input$med_federal_debt 
  
  
#STANDARD: Now we begin to get the values for the standard output
  debt_payment <- pay.per.year(debt_total, input$avg_interest_rate, res, years = 20)
  payments_output_standard <- payments(grow(debt_total, input$avg_interest_rate, n = res), input$avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
    for (i in 1:res) {debt_payment_standard[i] <- 0}
    for (k in (res+1):20) {debt_payment_standard[k] <- debt_payment}
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  debt_left_standard <- vector(length = 20)
  debt_left_standard[1] <- grow(debt_total, input$avg_interest_rate)
    for (i in 2:res) {debt_left_standard[i] <- grow(debt_left_standard[i-1], input$avg_interest_rate)}
    for (k in (res+1):20) {debt_left_standard[k] <- payments_output_standard[k-res]}
  disposable_standard <- vector(length = 20)
    for (i in 1:res){disposable_standard[i] <- gross[i]*(1-input$residency_tax)-debt_payment_standard[i]}
    for (k in (res+1):20){disposable_standard[k] <- gross[k]*(1-input$attending_tax)-debt_payment_standard[k]}
  cum_disposable_standard <- cumsum(disposable_standard)
    
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
    
    
#10-YEAR LOAN REPAYMENT OPTION- values calculated below, first the private payments then the federal ones
  #We will be modeling paying the private loans off over 20 years after medschool (but waiting until after residency to begin paying)
  priv.payment <- pay.per.year(private.total, input$avg_interest_rate, res, years = 20)
  stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {stand.repay[k] <- priv.payment}
  priv.remaining <- vector(length=20)
    for(i in 1:res) {priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
  priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)}
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross[1:10]*0.1 #this is the annual payment (10% of income)
  percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0) #it needs to be a 20-year vector, but they will stop paying at year 5
  forgiven.amount.ten <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount.ten, input$growth_rate, input$attending_tax, res, years = 10, print.saved=0)
    prep.payment <- vector(length = 10)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):10){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_ten<- vector(length = 10)
  forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
  forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  #combining the two above repayments (fed and private) below
  prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
  fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) 
  payment_ten <- vector(length = 20)
    for(i in 1:20) {payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_ten[10] <- payment_ten[10] + forgiven.amount.ten*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_ten <- cumsum(payment_ten) #cumulative payments
  disposable_ten <-  vector(length = 20)
    for (i in 1:res){disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]}
    for (k in (res+1):20){disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]}
    disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount.ten*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_ten <- cumsum(disposable_ten)
  debt_left_ten <- priv.remaining + fed.remaining 
    
  ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
    
    
#20 YEAR REPAYMENT OPTION
  #Just as with the 10-year repayment, we will need to handle the private (not forgivable) loans first
  #the code for this is already calculated above
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross*0.1
  forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, input$growth_rate, input$attending_tax, res, print.saved=0)
  prep.payment <- vector(length = 20)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):20){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_twenty<- vector(length = 20)
  forgiveness.fund.annual_twenty[1:res] <- 0
    for(i in (res+1):20) {forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
    forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
    #combine the two repayments (fed and private) below
  payment_twenty <- vector(length = 20)
    for(i in 1:20) {payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
    payment_twenty[20] <- payment_twenty[20] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
  disposable_twenty <-  vector(length = 20)
    for (i in 1:res){disposable_twenty[i] <- gross[i]*(1-input$residency_tax)-payment_twenty[i]}
    for (k in (res+1):19){disposable_twenty[k] <- gross[k]*(1-input$attending_tax)-payment_twenty[k]}
    disposable_twenty[20] <- gross[20]*(1-input$attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_twenty <- cumsum(disposable_twenty)
  debt_left_twenty <- priv.remaining + fed.remaining 
    
  twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)
    
    
#UNDERSERVED REPAYMENT OPTION
  total.debt.underserved <- input$undergrad_private_debt + input$undergrad_federal_debt 
  underserved.payment <- pay.per.year(total.debt.underserved, input$avg_interest_rate, res, years = 20)
  underserved.repay <- vector(length=20)
    for(i in 1:res) {underserved.repay[i]=0}
    for(k in (res+1):20) {underserved.repay[k] <- underserved.payment}
  underserved.remaining <- vector(length=20)
    for(i in 1:res) {underserved.remaining[i] <- grow(total.debt.underserved, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
    underserved.remaining[k] <- underserved.remaining[k]*(1+input$avg_interest_rate)}
  disposable_underserved <-  vector(length = 20)
    for (i in 1:res){disposable_underserved[i] <- gross[i]*(1-input$residency_tax)}
    for (k in (res+1):20){disposable_underserved[k] <- gross[k]*(1-input$attending_tax)-underserved.repay[k]}
  cum_disposable_underserved <- cumsum(disposable_underserved)
  total_paid_underserved <- cumsum(underserved.repay)
    
  underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
    
    
#MILITARY REPAYMENT OPTION
  total.debt.military <- input$undergrad_private_debt + input$undergrad_federal_debt 
  military.payment <- pay.per.year(total.debt.military, input$avg_interest_rate, res, years = 20)
  military.repay <- vector(length=20)
    for(i in 1:res) {military.repay[i]=0}
    for(k in (res+1):20) {military.repay[k] <- military.payment}
  military.remaining <- vector(length=20)
    for(i in 1:res) {military.remaining[i] <- grow(total.debt.military, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {military.remaining[k] <- (military.remaining[k-1] - military.payment)
  military.remaining[k] <- military.remaining[k]*(1+input$avg_interest_rate)}
  disposable_military <-  vector(length = 20)
    for (i in 1:res){disposable_military[i] <- military.gross[i]*(1-input$residency_tax)}
    disposable_military[1] <- disposable_military[1]+50000 #need to add 50k for signing bonus and UG salary
    for (k in (res+1):20){disposable_military[k] <- military.gross[k]*(1-input$attending_tax)-military.repay[k]}
  cum_disposable_military <- cumsum(disposable_military)
  total_paid_military <- cumsum(military.repay)
    
  military_frame <- data.frame(years, military.gross, disposable_military, cum_disposable_military, military.remaining, military.repay, total_paid_military)
    
    
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
    
    p3 <- ggplot(twenty_year_frame, aes(x = years)) +
      geom_line(aes(y = cum_disposable_twenty, color = "Cumulative Disposable Income")) +
      geom_line(aes(y = total_paid_twenty, color = "Total Debt Paid")) +
      scale_color_manual("",
                         breaks = c("Cumulative Disposable Income", "Total Debt Paid"),
                         values = c("Cumulative Disposable Income" = "green", "Total Debt Paid" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("20-Year 10% Repayment Option") +
        theme_minimal()
    
    p4 <- ggplot(underserved_frame, aes(x = years)) +
      geom_line(aes(y = cum_disposable_underserved, color = "Cumulative Disposable Income")) +
      geom_line(aes(y = total_paid_underserved, color = "Total Debt Paid")) +
      scale_color_manual("",
                         breaks = c("Cumulative Disposable Income", "Total Debt Paid"),
                         values = c("Cumulative Disposable Income" = "green", "Total Debt Paid" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("NHSC (Underserved) Repayment Option") +
        theme_minimal()
    
    p5 <- ggplot(military_frame, aes(x = years)) +
      geom_line(aes(y = cum_disposable_military, color = "Cumulative Disposable Income")) +
      geom_line(aes(y = total_paid_military, color = "Total Debt Paid")) +
      scale_color_manual("",
                         breaks = c("Cumulative Disposable Income", "Total Debt Paid"),
                         values = c("Cumulative Disposable Income" = "green", "Total Debt Paid" = "red")) +
      scale_y_continuous(labels = comma) +
        xlab("Years After Medical School Graduation") +
        ylab("Dollars") +
        ggtitle("Military Repayment Option") +
        theme_minimal()
    
    ggarrange(p1, p2, p3, p4, p5)
    
  })


  
  
  
output$Repayments <- renderUI({
#The following print out is to direct the reader's attention to the words at the bottom of the shiny, not the numbers at the top
  cat("---------------------------------------------------------")
  cat("Please scroll to the bottom to see a description of your repayment plans ")
  cat("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")

  
#First, we will initialize values that are used throughout (ex. salary, debt, interest rate, residency lenghth, etc. This occurs immediately below)
  res <- input$PGY_education
  salary <- input$avg_attending_salary
  in.military <- c(87965, 99726.5, 115671.5, 128496.5, 136504, 141026, 147916.5, 153001, 156988.5, 163579, 168533.5, 176593, 180956, 183542, 187181.5, 198565, 199579.5, 206582, 213525.5, 206792.5) #filling in the military salary expected for the first 20 years as a military doc
  military.gross <- vector(length=20) #the below if statement is due to military policy. 5 years or less of residency and you owe them 4 years, anything past 5 and you owe them the length of the residency less one year
    if(res<=5) {for(i in 1:(res+4)) {military.gross[i] <- in.military[i]}#military residency+service years salary
                for(j in (res+5):20) {military.gross[j] <- input$avg_attending_salary} }#civilian physician salary
    if(res>5) {for(i in 1:(res+(res-1))) {military.gross[i] <- in.military[i]}#military residency+service years salary
               for(j in (res+res):20) {military.gross[j] <- input$avg_attending_salary}} #civilian physician salary
  years <- c(1:20)
  gross <- vector(length = 20)
    for (i in 1:res) {gross[i] <- input$avg_residency_salary}
    for (k in (res+1):20) {gross[k] <- salary}
  debt_total <- input$undergrad_federal_debt + input$undergrad_private_debt + input$med_federal_debt + input$med_private_debt
  private.total <- input$undergrad_private_debt + input$med_private_debt
  federal.total <- input$undergrad_federal_debt + input$med_federal_debt 
  
  
#STANDARD: Now we begin to get the values for the standard output
  debt_payment <- pay.per.year(debt_total, input$avg_interest_rate, res, years = 20)
  payments_output_standard <- payments(grow(debt_total, input$avg_interest_rate, n = res), input$avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
    for (i in 1:res) {debt_payment_standard[i] <- 0}
    for (k in (res+1):20) {debt_payment_standard[k] <- debt_payment}
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  debt_left_standard <- vector(length = 20)
    debt_left_standard[1] <- grow(debt_total, input$avg_interest_rate)
    for (i in 2:res) {debt_left_standard[i] <- grow(debt_left_standard[i-1], input$avg_interest_rate)}
    for (k in (res+1):20) {debt_left_standard[k] <- payments_output_standard[k-res]}
  disposable_standard <- vector(length = 20)
    for (i in 1:res){disposable_standard[i] <- gross[i]*(1-input$residency_tax)-debt_payment_standard[i]}
    for (k in (res+1):20){disposable_standard[k] <- gross[k]*(1-input$attending_tax)-debt_payment_standard[k]}
  cum_disposable_standard <- cumsum(disposable_standard)
  
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
  
  
#10-YEAR LOAN REPAYMENT OPTION- values calculated below, first the private payments then the federal ones
  #We will be modeling paying the private loans off over 20 years after medschool (but waiting until after residency to begin paying)
  priv.payment <- pay.per.year(private.total, input$avg_interest_rate, res, years = 20)
  stand.repay <- vector(length=20)
    for(i in 1:res) {stand.repay[i]=0}
    for(k in (res+1):20) {stand.repay[k] <- priv.payment}
  priv.remaining <- vector(length=20)
    for(i in 1:res) {priv.remaining[i] <- grow(private.total, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
                       priv.remaining[k] <- priv.remaining[k]*(1+input$avg_interest_rate)}
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross[1:10]*0.1 #this is the annual payment (10% of income)
  percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0) #it needs to be a 20-year vector, but they will stop paying at year 5
  forgiven.amount.ten <- income.driven(federal.total, input$avg_interest_rate, income=gross, plan = 10) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount.ten, input$growth_rate, input$attending_tax, res, years = 10, print.saved=0)
  prep.payment <- vector(length = 10)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):10){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_ten<- vector(length = 10)
  forgiveness.fund.annual_ten[1:res] <- 0
    for(i in (res+1):10) {forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
                         forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
  #combining the two above repayments (fed and private) below
  prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
  fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
    for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) 
  payment_ten <- vector(length = 20)
   for(i in 1:20) {payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
  payment_ten[10] <- payment_ten[10] + forgiven.amount.ten*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_ten <- cumsum(payment_ten) #cumulative payments
  disposable_ten <-  vector(length = 20)
    for (i in 1:res){disposable_ten[i] <- gross[i]*(1-input$residency_tax)-payment_ten[i]}
    for (k in (res+1):20){disposable_ten[k] <- gross[k]*(1-input$attending_tax)-payment_ten[k]}
  disposable_ten[10] <- gross[10]*(1-input$attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount.ten*input$attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_ten <- cumsum(disposable_ten)
  debt_left_ten <- priv.remaining + fed.remaining 
  
  ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
  
  
#20 YEAR REPAYMENT OPTION
  #Just as with the 10-year repayment, we will need to handle the private (not forgivable) loans first
  #the code for this is already calculated above
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  percent.payment <- gross*0.1
  forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, input$growth_rate, input$attending_tax, res, print.saved=0)
  prep.payment <- vector(length = 20)
    for(i in 1:res){prep.payment[i]=0}
    for(k in (res+1):20){prep.payment[k]=prep.payment.amount}
  fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year
  forgiveness.fund.annual_twenty<- vector(length = 20)
    forgiveness.fund.annual_twenty[1:res] <- 0
    for(i in (res+1):20) {forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
                         forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+input$growth_rate)} #*this would be the vector of how much you have saved to pay off the final forgiven amount
    for(i in 1:20) {if(fed.remaining[i] <= 0) {percent.payment[i]=0}}
  #combine the two repayments (fed and private) below
  payment_twenty <- vector(length = 20)
    for(i in 1:20) {payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]}
  payment_twenty[20] <- payment_twenty[20] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
  total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
  disposable_twenty <-  vector(length = 20)
    for (i in 1:res){disposable_twenty[i] <- gross[i]*(1-input$residency_tax)-payment_twenty[i]}
    for (k in (res+1):19){disposable_twenty[k] <- gross[k]*(1-input$attending_tax)-payment_twenty[k]}
  disposable_twenty[20] <- gross[20]*(1-input$attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_twenty <- cumsum(disposable_twenty)
  debt_left_twenty <- priv.remaining + fed.remaining 
  
  twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)
  
  
#UNDERSERVED REPAYMENT OPTION
  total.debt.underserved <- input$undergrad_private_debt + input$undergrad_federal_debt 
  underserved.payment <- pay.per.year(total.debt.underserved, input$avg_interest_rate, res, years = 20)
  underserved.repay <- vector(length=20)
    for(i in 1:res) {underserved.repay[i]=0}
    for(k in (res+1):20) {underserved.repay[k] <- underserved.payment}
  underserved.remaining <- vector(length=20)
    for(i in 1:res) {underserved.remaining[i] <- grow(total.debt.underserved, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
                        underserved.remaining[k] <- underserved.remaining[k]*(1+input$avg_interest_rate)}
  disposable_underserved <-  vector(length = 20)
    for (i in 1:res){disposable_underserved[i] <- gross[i]*(1-input$residency_tax)}
    for (k in (res+1):20){disposable_underserved[k] <- gross[k]*(1-input$attending_tax)-underserved.repay[k]}
  cum_disposable_underserved <- cumsum(disposable_underserved)
  total_paid_underserved <- cumsum(underserved.repay)
  
  underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
  
    
#MILITARY REPAYMENT OPTION
  total.debt.military <- input$undergrad_private_debt + input$undergrad_federal_debt 
  military.payment <- pay.per.year(total.debt.military, input$avg_interest_rate, res, years = 20)
  military.repay <- vector(length=20)
    for(i in 1:res) {military.repay[i]=0}
    for(k in (res+1):20) {military.repay[k] <- military.payment}
  military.remaining <- vector(length=20)
    for(i in 1:res) {military.remaining[i] <- grow(total.debt.military, input$avg_interest_rate, n=i)}
    for(k in (res+1):20) {military.remaining[k] <- (military.remaining[k-1] - military.payment)
                         military.remaining[k] <- military.remaining[k]*(1+input$avg_interest_rate)}
  disposable_military <-  vector(length = 20)
    for (i in 1:res){disposable_military[i] <- military.gross[i]*(1-input$residency_tax)}
    disposable_military[1] <- disposable_military[1]+50000 #need to add 50k for signing bonus and UG salary
    for (k in (res+1):20){disposable_military[k] <- military.gross[k]*(1-input$attending_tax)-military.repay[k]}
  cum_disposable_military <- cumsum(disposable_military)
  total_paid_military <- cumsum(military.repay)
  
  military_frame <- data.frame(years, military.gross, disposable_military, cum_disposable_military, military.remaining, military.repay, total_paid_military)
  
  
#Word form output is put below here (explaining the results in a paragraph for each option)  
  #cat("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  HTML(
    paste0("Using the ", "<b>", "standard repayment plan ", "</b>",
           "(in which you pay back the entirety of your loans), you will pay $",
           format(round(total_paid_standard[20], 2), nsmall = 2),
           " in total. This will work out to an average of $",
           format(round(stand.repay[1], 2), nsmall = 2),
           " per year during residency and $",
           format(round(stand.repay[9], 2), nsmall = 2),
           " during attendinghood. After taxes this means you will make an average of $",
           format(round(disposable_standard[2], 2), nsmall = 2),
           " per year during residency, and $",
           format(round(disposable_standard[9], 2), nsmall = 2),
           " per year during attendinghood (that is salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $",
           format(round(cum_disposable_standard[20], 2), nsmall = 2), ".", '<br/>',
           '<br/>', "Using the ", "<b>", "10-year repayment plan ", "</b>",
           "(in which you must work at a 401(c) and then must pay back the entirety of your private loans and pay 10% of your salary for 10 years), you will pay $",
           format(round(total_paid_ten[20], 2), nsmall = 2),
           " in total. This will work out to an average of $",
           format(round(payment_ten[1], 2), nsmall = 2),
           " per year during residency, $",
           format(round(payment_ten[9], 2), nsmall = 2),
           " during attendinghood, and $",
           format(round(payment_ten[19], 2), nsmall = 2),
           " after completing your 10 years of repayments. After taxes this means you will make an average of $",
           format(round(disposable_ten[2], 2), nsmall = 2),
           " per year during residency, $",
           format(round(disposable_ten[9], 2), nsmall = 2),
           " per year during attendinghood, and $",
           format(round(disposable_ten[19], 2), nsmall = 2),
           " after completing your 10 years repaying 10% of your income (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $",
           format(round(cum_disposable_ten[20], 2), nsmall = 2), ".", '<br/>',
           '<br/>', "Using the ", "<b>", "20-year repayment plan ", "</b>",
           "(in which you must pay back the entirety of your private loans and pay 10% of your salary for 20 years), you will pay $",
           format(round(total_paid_twenty[20], 2), nsmall = 2),
           " in total. This will work out to an average of $",
           format(round(payment_twenty[1], 2), nsmall = 2),
           " per year during residency and $",
           format(round(payment_twenty[19], 2), nsmall = 2),
           " during attendinghood. After taxes this means you will make an average of $",
           format(round(disposable_twenty[2], 2), nsmall = 2),
           " per year during residency and $",
           format(round(disposable_twenty[19], 2), nsmall = 2),
           " per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $",
           format(round(cum_disposable_twenty[20], 2), nsmall = 2), ".", '<br/>',
           '<br/>', "If you ", "<b>", "join the NHSC", "</b>",
           ", they will pay for your medical education. This means we removed all of your medical loans on presenting this option. Note that this program is competitive and that to qualify, you will need to 1) commit to enter a primary care specialty, and 2) commit to provide care in an underserved community. You will still have to pay off your undergrad loans however. This means you will pay $",
           format(round(total_paid_underserved[20], 2), nsmall = 2),
           " in total. This will work out to an average of $",
           format(round(underserved.payment[1], 2), nsmall = 2),
           " per year during residency and $",
           format(round(underserved.payment[9], 2), nsmall = 2),
           " during attendinghood. After taxes this means you will make an average of $",
           format(round(disposable_underserved[2], 2), nsmall = 2),
           " per year during residency and $",
           format(round(disposable_underserved[19], 2), nsmall = 2),
           " per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $",
           format(round(cum_disposable_underserved[20], 2), nsmall = 2), ".", '<br/>',
           '<br/>', "If you ", "<b>", "join the military", "</b>",
           ", they will pay for your medical education. This means we removed all of your medical loans on presenting this option. Note that committing to the military comes with a complex comitment including a different match process and 4 to 6 years of military service owed after your residency (you either owe 4 years or the number of years of residency minus 1 year, whichever is larger). You will still have to pay off your undergrad loans however. This means you will pay $",
           format(round(total_paid_military[20], 2), nsmall = 2),
           " in total. This will work out to an average of $",
           format(round(military.payment[1], 2), nsmall = 2),
           " per year during residency and $", 
           format(round(military.payment[19], 2), nsmall = 2),
           " during attendinghood. After taxes this means you will make more money during each year in the military, including some money during medical school (shown here by increasing year 1 earnings by $50k). You will roughly earn $",
           format(round(disposable_military[5], 2), nsmall = 2),
           " during each year in the military and $",
           format(round(disposable_military[19], 2), nsmall = 2),
           " per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $",
           format(round(cum_disposable_military[20], 2), nsmall = 2), "."
    )
  )
  #cat("Using the standard repayment plan (in which you pay back the entirety of your loans), you will pay $", total_paid_standard[20], "in total. This will work out to an average of $", stand.repay[1], "per year during residency and $", stand.repay[9], "during attendinghood. After taxes this means you will make an average of $", disposable_standard[2], " per year during residency, and $", disposable_standard[9], "per year during attendinghood (that is salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $", cum_disposable_standard[20], ".")
  #cat("--------------------------------------------------------------------------------------------------------")
  #cat("Using the 10-year repayment plan (in which you must work at a 401(c) and then must pay back the entirety of your private loans and pay 10% of your salary for 10 years), you will pay $", total_paid_ten[20], "in total. This will work out to an average of $", payment_ten[1], "per year during residency, $", payment_ten[9], "during attendinghood, and", payment_ten[19], "after completing your 10-years of repayments. After taxes this means you will make an average of $", disposable_ten[2], " per year during residency, $", disposable_ten[9], "per year during attendinghood, and $", disposable_ten[19], "after completing your 10 years repaying 10% of your income (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $", cum_disposable_ten[20], ".")
  #cat("--------------------------------------------------------------------------------------------------------")
  #cat("Using the 20-year repayment plan (in which you must pay back the entirety of your private loans and pay 10% of your salary for 20 years), you will pay $", total_paid_twenty[20], "in total. This will work out to an average of $", payment_twenty[1], "per year during residency and $", payment_twenty[19], "during attendinghood. After taxes this means you will make an average of $", disposable_twenty[2], " per year during residency and $", disposable_twenty[19], "per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $", cum_disposable_twenty[20], ".")
  #cat("--------------------------------------------------------------------------------------------------------")
  #cat("If you join the NHSC, they will pay for your medical education. This means we removed all of your medical loans on presenting this option. Note that this program is competitive and that to qualify, you will need to 1) commit to enter a primary care specialty, and 2) commit to provide care in an underserved community. You will still have to pay off your undergrad loans however. This means you will pay $", total_paid_underserved[20], "in total. This will work out to an average of $", underserved.payment[1], "per year during residency and $", underserved.payment[9], "during attendinghood. After taxes this means you will make an average of $", disposable_underserved[2], " per year during residency and $", disposable_underserved[19], "per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $", cum_disposable_underserved[20], ".")
  #cat("--------------------------------------------------------------------------------------------------------")
  #cat("If you join the military, they will pay for your medical education. This means we removed all of your medical loans on presenting this option. Note that committing to the military comes with a complex comitment including a different match process and 4-6 years of military service owed after your residency (you either owe 4 years or the number of years of residency-1 year, whichever is larger). You will still have to pay off your undergrad loans however. This means you will pay $", total_paid_military[20], "in total. This will work out to an average of $", military.payment[1], "per year during residency and $", military.payment[19], "during attendinghood. After taxes this means you will make more money during each year in the military, including some money during medical school (shown here by increasing year 1 earnings by 50k). You will roughly earn $", disposable_military[5], " during each year in the military and $", disposable_military[19], "per year during attendinghood (these values are your salary after removing taxes and debt payments). In your first 20 years after med school, after taxes and debt repayments, you will make a total of $", cum_disposable_military[20], ".")
      })
}


shinyApp(ui = ui, server = server)