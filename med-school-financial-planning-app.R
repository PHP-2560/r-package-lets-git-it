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



specialty_res <- function(specialty) {
  res <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Years of Training`)
  print(as.numeric(res))
}

res <- specialty_res("Psychiatry")

specialty_salary <- function(specialty) {
  salary <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Annual Salary`)
  print(as.numeric(salary))
}

salary <- specialty_salary("Psychiatry")

grow <- function(base, interest, n=1) { #grow takes the base value and grows or shrinks it by the interest rate (which should be .07 if the rate is 7%), n is how many times it should compound
  if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
    interest = interest/100
  }
  
  for(i in 1:n){#grows it by the rate for each iteration (n many iterations)
    base = base*(1+interest)
  }
  return(base)
}

grow(10,7,10)

#how much to pay per year:
#This will take the debt total grown after res and calculate how much to pay per year to pay off loan after res
pay.per.year <- function(debt.total, interest, years =20-res) {
  debt.total = grow(debt.total, interest, n = res)
  testpay = debt.total*interest*3
  for(k in 1:20) {testpay <- testpay*0.95
  DT <- debt.total
  for(i in 1:years) {
    DT <- DT - testpay
    DT <- DT*(1+interest)
  }
  if(length(payments(debt.total, interest, testpay)) >= years){ break} #we are trying to stop it when the repayment plan finishes at the right time
  } 
  
  print(testpay) #prints how much you need to pay per year
}

pay.per.year(600000, .075, years=16) #this pay.per.year gives us a fixed number for the person to pay during each year of their attendinghood

payments <- function(base, interest, payment){ #annual payment made is payment, base is the debt, interest is the rate at which the debt grows
  if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
    interest = interest/100
  }
  
  annual.debt <- numeric() #initialize the annual.debt vector
  for(i in 1:100){
    base <- (base-payment)*(1+interest) 
    annual.debt <- c(annual.debt, base)
    n<-i
    if(base<payment) break #break when remaining amount left to be paid is less than our payment amount
  }
  if(annual.debt[n]< payment) annual.debt <- c(annual.debt, 0)
  total.pay = n*payment + annual.debt[n] #in addition to making the consistent annual payments, we need to add on the final payment which is less than the value we pay annually
  print(n+1) #print the number of payments we made
  print(total.pay) #print the total payments we made
  print(annual.debt) #print out the debt remaining each year
  
}


payments(100, .07, 12)#testing our function
  
years <- c(1:20)
gross <- vector(length = 20)
for (i in 1:res) {
  gross[i] <- 65000
}
for (k in res+1:20) {
  gross[k] <- salary
}
gross


debt_total <- total_federal_debt + total_private_debt

###TESTING
debt_total <- 300000
avg_interest_rate <- 0.076

debt_payment <- pay.per.year(debt_total, avg_interest_rate)
payments_output_standard <- payments(grow(debt_total, avg_interest_rate, n = res), avg_interest_rate, debt_payment)
debt_payment_standard <- vector(length = 20)
for (i in 1:res) {
  debt_payment_standard[i] <- 0
}
for (k in res+1:20) {
  debt_payment_standard[k] <- debt_payment
}
total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments

debt_left_standard <- vector(length = 20)
debt_left_standard[1] <- grow(debt_total, avg_interest_rate)
for (i in 2:res) {
  debt_left_standard[i] <- grow(debt_left_standard[i-1], avg_interest_rate)
}
for (k in res+1:20) {
  debt_left_standard[k] <- payments_output_standard[k-res]
}
debt_left_standard

disposable <- vector(length = 20)

disposable <- c(1, 2, 3, 4, 5, 6, 7)

residency_tax <- 0.25
attending_tax <- 0.35

disposable_standard <- vector(length = 20)
for (i in 1:res){
  disposable_standard[i] <- gross[i]*(1-residency_tax)-debt_payment_standard[i]
}
for (k in res+1:20){
  disposable_standard[k] <- gross[k]*(1-attending_tax)-debt_payment_standard[k]
}
disposable_standard


cum_disposable_standard <- cumsum(disposable_standard)


standard_frame <- data.frame(years, gross[1:20], disposable_standard[1:20], cum_disposable_standard[1:20], debt_left_standard[1:20], debt_payment_standard[1:20], total_paid_standard[1:20])
ten_year_frame <- data.frame(years, gross)
twenty_year_frame <- data.frame(years, gross)
military_frame <- data.frame(years, gross)
underserved_frame <- data.frame(years, gross)

server <- function(input, output) {
  #x-axis of PGY years, y-axis of dollars
  #1 color is gross income (stop at 20 years)
  #1 color is disposale income (post-taxes and debt repayment)
  #1 color is debt repayment per year
  specialty_res <- function(specialty) {
    res <- specialty_info %>%
      filter(Specialty == specialty) %>%
      select(`Years of Training`)
    print(as.numeric(res))
  }
  
  res <- specialty_res("Psychiatry")
  
  specialty_salary <- function(specialty) {
    salary <- specialty_info %>%
      filter(Specialty == specialty) %>%
      select(`Annual Salary`)
    print(as.numeric(salary))
  }
  
  salary <- specialty_salary("Psychiatry")
  
  grow <- function(base, interest, n=1) { #grow takes the base value and grows or shrinks it by the interest rate (which should be .07 if the rate is 7%), n is how many times it should compound
    if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
      interest = interest/100
    }
    
    for(i in 1:n){#grows it by the rate for each iteration (n many iterations)
      base = base*(1+interest)
    }
    return(base)
  }
  
  grow(10,7,10)
  
  #how much to pay per year:
  #This will take the debt total grown after res and calculate how much to pay per year to pay off loan after res
  pay.per.year <- function(debt.total, interest, years =20-res) {
    debt.total = grow(debt.total, interest, n = res)
    testpay = debt.total*interest*3
    for(k in 1:20) {testpay <- testpay*0.95
    DT <- debt.total
    for(i in 1:years) {
      DT <- DT - testpay
      DT <- DT*(1+interest)
    }
    if(length(payments(debt.total, interest, testpay)) >= years){ break} #we are trying to stop it when the repayment plan finishes at the right time
    } 
    
    print(testpay) #prints how much you need to pay per year
  }
  
  pay.per.year(600000, .075, years=16) #this pay.per.year gives us a fixed number for the person to pay during each year of their attendinghood
  
  payments <- function(base, interest, payment){ #annual payment made is payment, base is the debt, interest is the rate at which the debt grows
    if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
      interest = interest/100
    }
    
    annual.debt <- numeric() #initialize the annual.debt vector
    for(i in 1:100){
      base <- (base-payment)*(1+interest) 
      annual.debt <- c(annual.debt, base)
      n<-i
      if(base<payment) break #break when remaining amount left to be paid is less than our payment amount
    }
    if(annual.debt[n]< payment) annual.debt <- c(annual.debt, 0)
    total.pay = n*payment + annual.debt[n] #in addition to making the consistent annual payments, we need to add on the final payment which is less than the value we pay annually
    print(n+1) #print the number of payments we made
    print(total.pay) #print the total payments we made
    print(annual.debt) #print out the debt remaining each year
    
  }
  
  
  payments(100, .07, 12)#testing our function
  
  years <- c(1:20)
  gross <- vector(length = 20)
  for (i in 1:res) {
    gross[i] <- 65000
  }
  for (k in res+1:20) {
    gross[k] <- salary
  }
  gross
  
  
  debt_total <- total_federal_debt + total_private_debt
  
  ###TESTING
  debt_total <- 300000
  avg_interest_rate <- 0.076
  
  debt_payment <- pay.per.year(debt_total, avg_interest_rate)
  payments_output_standard <- payments(grow(debt_total, avg_interest_rate, n = res), avg_interest_rate, debt_payment)
  debt_payment_standard <- vector(length = 20)
  for (i in 1:res) {
    debt_payment_standard[i] <- 0
  }
  for (k in res+1:20) {
    debt_payment_standard[k] <- debt_payment
  }
  total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments
  
  debt_left_standard <- vector(length = 20)
  debt_left_standard[1] <- grow(debt_total, avg_interest_rate)
  for (i in 2:res) {
    debt_left_standard[i] <- grow(debt_left_standard[i-1], avg_interest_rate)
  }
  for (k in res+1:20) {
    debt_left_standard[k] <- payments_output_standard[k-res]
  }
  debt_left_standard
  
  disposable <- vector(length = 20)
  
  disposable <- c(1, 2, 3, 4, 5, 6, 7)
  
  residency_tax <- 0.25
  attending_tax <- 0.35
  
  disposable_standard <- vector(length = 20)
  for (i in 1:res){
    disposable_standard[i] <- gross[i]*(1-residency_tax)-debt_payment_standard[i]
  }
  for (k in res+1:20){
    disposable_standard[k] <- gross[k]*(1-attending_tax)-debt_payment_standard[k]
  }
  disposable_standard
  
  cum_disposable_standard <- cumsum(disposable_standard)
  standard_frame <- data.frame(years, gross, disposable_standard, cumulative_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)

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