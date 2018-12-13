

res <- 3
avg_residency_salary <- 65000
avg_attending_salary <- 230000
salary <- 230000
avg_interest_rate <- .076
attending_tax <- 0.35
residency_tax <- 0.25
growth_rate <-.05
res <- 3
years <- c(1:20)
undergrad_private_debt <- 100000
undergrad_federal_debt <- 100000
med_private_debt <- 100000
med_federal_debt <- 100000

  
  in.military <- c(87965, 99726.5, 115671.5, 128496.5, 136504, 141026, 147916.5, 153001, 156988.5, 163579, 168533.5, 176593, 180956, 183542, 187181.5, 198565, 199579.5, 206582, 213525.5, 206792.5)
  military.gross <- vector(length=20)
  
  if(res<=5) {
    for(i in 1:(res+4)) {military.gross[i] <- in.military[i]}#military residency salary}
    for(j in (res+5):20) {military.gross[j] <- avg_attending_salary}#civilian physician salary}
  }
  if(res>5) {
    for(i in 1:(res+(res-1))) {military.gross[i] <- in.military[i]}#military physician salary}
    for(j in (res+res):20) {military.gross[j] <- avg_attending_salary}#civilian physician salary}
  }  
  
  years <- c(1:20)
  gross <- vector(length = 20)
  for (i in 1:res) {
    gross[i] <- avg_residency_salary
  }
  for (k in (res+1):20) {
    gross[k] <- salary
  }
  
  debt_total <- undergrad_federal_debt + undergrad_private_debt + med_federal_debt + med_private_debt
  
  debt_payment <- pay.per.year(debt_total, avg_interest_rate)
  payments_output_standard <- payments(grow(debt_total, avg_interest_rate, n = res), avg_interest_rate, debt_payment)
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
  debt_left_standard[1] <- grow(debt_total, avg_interest_rate)
  for (i in 2:res) {
    debt_left_standard[i] <- grow(debt_left_standard[i-1], avg_interest_rate)
  }
  for (k in (res+1):20) {
    debt_left_standard[k] <- payments_output_standard[k-res]
  }
  debt_left_standard
  
  disposable_standard <- vector(length = 20)
  for (i in 1:res){
    disposable_standard[i] <- gross[i]*(1-residency_tax)-debt_payment_standard[i]
  }
  for (k in (res+1):20){
    disposable_standard[k] <- gross[k]*(1-attending_tax)-debt_payment_standard[k]
  }
  disposable_standard
  
  cum_disposable_standard <- cumsum(disposable_standard)
  
  standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)
  
  #10-YEAR LOAN REPAYMENT OPTION
  private.total <- undergrad_private_debt + med_private_debt #need to make this a thing
  
  federal.total <- undergrad_federal_debt + med_federal_debt #need this value
  
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
    priv.remaining[k] <- priv.remaining[k]*(1+avg_interest_rate)
  }
  
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  #gross should have already been called
  percent.payment <- gross[1:10]*0.1
  percent.payment <- c(percent.payment, 0,0,0,0,0,0,0,0,0,0)
  forgiven.amount <- income.driven(federal.total, avg_interest_rate, income=gross, plan = 10) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, growth_rate, attending_tax, years = 10, print.saved=0)
  prep.payment <- vector(length = 10)
  for(i in 1:res){
    prep.payment[i]=0 
  }
  for(k in (res+1):10){
    prep.payment[k]=prep.payment.amount
  }
  
  fed.remaining <- income.driven.vector(federal.total, avg_interest_rate, income=gross, plan = 10) #this gives how much of the fed debt is present each year
  
  
  forgiveness.fund.annual_ten<- vector(length = 10)
  forgiveness.fund.annual_ten[1:res] <- 0
  for(i in (res+1):10) {
    forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i-1]  + prep.payment.amount
    forgiveness.fund.annual_ten[i] = forgiveness.fund.annual_ten[i]*(1+growth_rate)
  } #*this would be the vector of how much you have saved to pay off the final forgiven amount
  
  #combine the two repayments (fed and private) below
  prep.payment <- c(prep.payment, 0,0,0,0,0,0,0,0,0,0)
  fed.remaining <- c(fed.remaining, 0,0,0,0,0,0,0,0,0,0)
  for(i in 1:20) {
    if(fed.remaining[i] <= 0) {percent.payment[i]=0}
  }
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
  disposable_ten[10] <- gross[10]*(1-attending_tax)-stand.repay[10]-percent.payment[10]-prep.payment[10]-(forgiven.amount*attending_tax - forgiveness.fund.annual_ten[10]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_ten <- cumsum(disposable_ten)
  
  debt_left_ten <- priv.remaining + fed.remaining 
  
  ten_year_frame <- data.frame(years, gross, disposable_ten, cum_disposable_ten, debt_left_ten, payment_ten, total_paid_ten, forgiveness.fund.annual_ten)
  
  #20 YEAR REPAYMENT OPTION
  private.total <- undergrad_private_debt + med_private_debt #need to make this a thing
  
  federal.total <- undergrad_federal_debt + med_federal_debt #need this value
  
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
    priv.remaining[k] <- priv.remaining[k]*(1+avg_interest_rate)
  }
  
  #For the remaining federal debt they will be repaying 10% of their income for 20 years:
  #gross should have already been called
  percent.payment <- gross*0.1
  forgiven.amount <- income.driven(federal.total, avg_interest_rate, income=gross) 
  prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, growth_rate, attending_tax, print.saved=0)
  prep.payment <- vector(length = 20)
  for(i in 1:res){
    prep.payment[i]=0 
  }
  for(k in (res+1):20){
    prep.payment[k]=prep.payment.amount
  }
  
  fed.remaining <- income.driven.vector(federal.total, avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year
  
  
  forgiveness.fund.annual_twenty<- vector(length = 20)
  forgiveness.fund.annual_twenty[1:res] <- 0
  for(i in (res+1):20) {
    forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
    forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+growth_rate)
  } #*this would be the vector of how much you have saved to pay off the final forgiven amount
  
  for(i in 1:20) {
    if(fed.remaining[i] <= 0) {percent.payment[i]=0}
  }
  
  #combine the two repayments (fed and private) below
  payment_twenty <- vector(length = 20)
  for(i in 1:20) {
    payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
  }
  payment_twenty[20] <- payment_twenty[20] + forgiven.amount*attending_tax #on the final year you pay extra to the IRS
  total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
  disposable_twenty <-  vector(length = 20)
  for (i in 1:res){
    disposable_twenty[i] <- gross[i]*(1-residency_tax)-payment_twenty[i]
  }
  for (k in (res+1):19){
    disposable_twenty[k] <- gross[k]*(1-attending_tax)-payment_twenty[k]
  }
  disposable_twenty[20] <- gross[20]*(1-attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
  cum_disposable_twenty <- cumsum(disposable_twenty)
  
  debt_left_twenty <- priv.remaining + fed.remaining 
  
  twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)
  
  #UNDERSERVED REPAYMENT OPTION
  total.debt.underserved <- undergrad_private_debt + undergrad_federal_debt 
  
  underserved.payment <- pay.per.year(total.debt.underserved, avg_interest_rate)
  
  underserved.repay <- vector(length=20)
  for(i in 1:res) {underserved.repay[i]=0}
  for(k in (res+1):20) {
    underserved.repay[k] <- underserved.payment
  }
  
  underserved.remaining <- vector(length=20)
  for(i in 1:res) {
    underserved.remaining[i] <- grow(total.debt.underserved, avg_interest_rate, n=i)
  }
  for(k in (res+1):20) {
    underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
    underserved.remaining[k] <- underserved.remaining[k]*(1+avg_interest_rate)
  }
  
  #need disposable
  disposable_underserved <-  vector(length = 20)
  for (i in 1:res){
    disposable_underserved[i] <- gross[i]*(1-residency_tax)
  }
  for (k in (res+1):20){
    disposable_underserved[k] <- gross[k]*(1-attending_tax)-underserved.repay[k]
  }
  
  #need cumDisposable 
  cum_disposable_underserved <- cumsum(disposable_underserved)
  
  #debt_left is "underserved.remaining"
  #payment_underserved is "underserved.repay"
  #need cumUnderservedrepay
  total_paid_underserved <- cumsum(underserved.repay)
  
  underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
  underserved_frame
  
  total.debt.military <- undergrad_private_debt + undergrad_federal_debt 
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

standard_frame
