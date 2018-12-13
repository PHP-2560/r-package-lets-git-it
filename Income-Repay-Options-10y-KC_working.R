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
twenty_year_frame <- data.frame(years, gross)
military_frame <- data.frame(years, gross)
underserved_frame <- data.frame(years, gross)

ten_year_frame
