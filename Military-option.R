avg_interest_rate <- .075
military.gross <- c(75000, 75000, 75000, 75000, 100000, 100000, 100000, 100000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000)
attending_tax <- 0.35
residency_tax <- 0.25
growth_rate <-.05
res <- 4
years <- c(1:20)
#just initializing values above for debugging below

#STARTING WITH Military option:

#FOR PRIVATE DEBT

total.debt.military <- undergrad_private_debt + undergrad_federal_debt 
#this is the UG debt total
total.debt.military <- 00000 #initializing a value





#first things first, if they have private loans, they will need to repay these in the 'standard' way 
#*********************INSERT THIS REPAYMENT************
#create stand.repay vector (payment per year)
military.payment <- pay.per.year(total.debt.military, avg_interest_rate)

military.repay <- vector(length=20)
  for(i in 1:res) {military.repay[i]=0}
  for(k in (res+1):20) {
    military.repay[k] <- military.payment
    }

military.remaining <- vector(length=20)
  for(i in 1:res) {
    military.remaining[i] <- grow(total.debt.military, avg_interest_rate, n=i)
  }
  for(k in (res+1):20) {
    military.remaining[k] <- (military.remaining[k-1] - military.payment)
    military.remaining[k] <- military.remaining[k]*(1+avg_interest_rate)
  }


#need disposable
disposable_military <-  vector(length = 20)
for (i in 1:res){
  disposable_military[i] <- military.gross[i]*(1-residency_tax)
}
disposable_military[1] <- disposable_military[1]+50000 #need to add 50k for signing bonus and UG salary
for (k in (res+1):20){
  disposable_military[k] <- military.gross[k]*(1-attending_tax)-military.repay[k]
}

#need cumDisposable 
cum_disposable_military <- cumsum(disposable_military)

#debt_left is "military.remaining"
#payment_military is "military.repay"
#need cummilitaryrepay
total_paid_military <- cumsum(military.repay)


military_frame <- data.frame(years, military.gross, disposable_military, cum_disposable_military, military.remaining, military.repay, total_paid_military)
military_frame








military.gross <- vector(length=20)


if(res<=5) {
  for(i in 1:res) {military.gross[i] <- 0}#military residency salary}
  for(k in (res+1):(res+4)) {military.gross[k] <- 0}#military physician salary}
  for(l in (res+5):20) {military.gross[l] <- 0}#civilian physician salary}
}
if(res>5) {
  for(i in 1:res) {military.gross[i] <- 0}#military residency salary}
  for(k in (res+1):(res+(res-1))) {military.gross[k] <- 0}#military physician salary}
  for(l in (res+(res-1)):20) {military.gross[l] <- 0}#civilian physician salary}
}  


























#below are some functions needed for the above to run













#Next I will launch the needed function income.driven and forgiveness.prep.fund as well as pay.per.year
income.driven <- function(base, interest,income) {
  payment <- income/10 
  annual.debt <- vector(length=20)
  annual.debt[20] <- 0
  for(i in 1:20){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) {break}
  }
  forgiven.amount <- annual.debt[20]
  print(forgiven.amount) #how much is forgiven
}


income.driven.vector <- function(base, interest,income) { #income needs to be a 20 element vector
  payment <- income/10 
  annual.debt <- vector(length=20)
  annual.debt[20] <- 0
  for(i in 1:20){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) break
  }
  forgiven.amount <- annual.debt #whole vector, not just last value on this one
  print(forgiven.amount) 
}



forgiveness.prep.fund <- function(amount.forgiven, growth.rate, tax.rate, years =20-res, print.saved=1) {#determines how much you need to put away per year to cover your tax bill on the forgiven amount from income.driven or income.driven short (for income driven short you would need to change years to 10) --- I assume people aren't putting money toward this during residency
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



