#NOTE THIS IS BASICALLY THE SAME EXACT THING AS THE 10 YEAR OPTION EXCEPT DEFAULTS AND VECTORS FOR PUBLIC REPAYMENT (BUT NOT FOR PRIVATE) ARE CHANGED TO 10 YEARS INSTEAD OF 20

interest_rate_avg <- .075
gross <- c(65000, 65000, 65000, 65000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000)
attending_tax <- 0.35
residency_tax <- 0.25
growth_rate <-.05
res <- 4
years <- c(1:20)
#just initializing values above for debugging below

#STARTING WITH 20 YEAR:

#FOR PRIVATE DEBT
private.total <- undergrad_private_debt + med_private_debt #need to make this a thing
private.total <- 100000 #initializing a value to work with

federal.total <- undergrad_federal_debt + med_federal_debt #need this value
federal.total <- 400000 #initializing a value to work with


#Next I will launch the needed function income.driven and forgiveness.prep.fund as well as pay.per.year
income.driven <- function(base, interest,income) {
  payment <- income/10 
  annual.debt <- vector(length=10)
  annual.debt[10] <- 0
  for(i in 1:10){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) {break}
  }
  forgiven.amount <- annual.debt[10]
  print(forgiven.amount) #how much is forgiven
}


income.driven.vector <- function(base, interest,income) { #income needs to be a 20 element vector
  payment <- income/10 
  annual.debt <- vector(length=10)
  annual.debt[10] <- 0
  for(i in 1:10){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) break
  }
  forgiven.amount <- annual.debt #whole vector, not just last value on this one
  print(forgiven.amount) 
}



forgiveness.prep.fund_ten <- function(amount.forgiven, growth.rate, tax.rate, years =10-res, print.saved=1) {#determines how much you need to put away per year to cover your tax bill on the forgiven amount from income.driven or income.driven short (for income driven short you would need to change years to 10) --- I assume people aren't putting money toward this during residency
  tax.debt <- amount.forgiven*tax.rate #this will show how much of your "forgiven" debt is now owed to the IRS
  test = tax.debt/(1.5*years)
  
  for(k in 1:20) {test = test*0.95
  saved =0
  for(i in 1:years) {
    saved = saved + test
    saved = saved*(1+growth.rate)
  }
  if(saved - tax.debt < 0){ break}
  } #*****NEED TO CHANGE IT TO ACCOUNT FOR CAPITAL GAINS TAX****
  if(print.saved==1) {
    print(saved)
  }
  print(test) #prints how much you need to save per year
} 

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


pay.per.year <- function(debt.total, interest, years =20-res) {
  debt.total = grow(debt.total, interest, n = res)
  testpay = debt.total*interest*3
  for(k in 1:20) {testpay <- testpay*0.95
    DT <- debt.total
    print(DT)
    print(testpay)
    for(i in 1:years) {
      DT <- DT - testpay
      DT <- DT*(1+interest)
    }
    debt.total.temp <- debt.total
  if(length(payments(debt.total.temp, interest, testpay)) >= years){ break} #we are trying to stop it when the repayment plan finishes at the right time
  } #*NEED TO MAKE SURE PAYMENTS FUNCTION DOESN'T ALTER THE DEBT.TOTAL VALUE, HENCE IT GETS A DIFFERENT INPUT
  
  print(testpay) #prints how much you need to pay per year
}








#first things first, if they have private loans, they will need to repay these in the 'standard' way 
#*********************INSERT THIS REPAYMENT************
#create stand.repay vector (payment per year)
priv.payment <- pay.per.year(private.total, avg_interest_rate)

stand.repay <- vector(length=20)
  for(i in 1:res) {stand.repay[i]=0}
  for(k in (res+1):20) {
    stand.repay[k] <- priv.payment
    }

priv.remaining <- vector(length=20)
  for(i in 1:res) {
    priv.remaining[i] <- grow(private.total, avg_interest_rate, n=i)
  }
  for(k in (res+1):20) {
    priv.remaining[k] <- (priv.remaining[k-1] - priv.payment)
    priv.remaining[k] <- priv.remaining[k]*(1+interest_rate_avg)
  }









#For the remaining federal debt they will be repaying 10% of their income for 20 years:
#gross should have already been called
percent.payment <- gross[1:10]*0.1
percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0)
forgiven.amount <- income.driven(federal.total, interest_rate_avg, income=gross) 
prep.payment.amount <- forgiveness.prep.fund_ten(forgiven.amount, growth_rate, attending_tax, print.saved=0)
prep.payment <- vector(length = 10)
  for(i in 1:res){
   prep.payment[i]=0 
  }
  for(k in (res+1):10){
    prep.payment[k]=prep.payment.amount
  }
  
fed.remaining <- income.driven.vector(federal.total, interest_rate_avg, income=gross) #this gives how much of the fed debt is present each year


forgiveness.fund.annual_ten<- vector(length = 10)
forgiveness.fund.annual_ten[1:res] <- 0
for(i in (res+1):10) {
  forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
  forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+growth_rate)
} #*this would be the vector of how much you have saved to pay off the final forgiven amount

























#combine the two repayments (fed and private) below
prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
forgiveness.fund.annual_ten<- c(forgiveness.fund.annual_ten, 0,0,0,0,0,0,0,0,0,0) #these three make the important vectors all go to 20 so they work nicely in the df

payment_ten <- vector(length = 20)
for(i in 1:20) {
 payment_ten[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
}
payment_ten[10] <- payment_ten[10] + forgiven.amount*attending_tax #on the final year you pay extra to the IRS
total_paid_ten <- cumsum(payment_ten) #cumulative payments
disposable_ten <-  vector(length = 20)
for (i in 1:res){
  disposable_ten[i] <- gross[i]*(1-residency_tax)-payment_ten[i]
}
for (k in (res+1):20){
  disposable_ten[k] <- gross[k]*(1-attending_tax)-payment_ten[k]
}
disposable_ten[10] <- gross[10]*(1-attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount*attending_tax - forgiveness.fund.annual[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
cum_disposable_ten <- cumsum(disposable_ten)

debt_left_ten <- priv.remaining + fed.remaining 


#***********May want to add a column to the df of the forgiveness repayment fund (i.e. a forgiveness.fund.annual vector)

percent.payment
stand.repay
prep.payment
forgiven.amount
payment_ten
total_paid_ten
disposable_ten
cum_disposable_ten
debt_left_ten


standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
twenty_year_frame <- data.frame(years, gross)
military_frame <- data.frame(years, gross)
underserved_frame <- data.frame(years, gross)

ten_year_frame
