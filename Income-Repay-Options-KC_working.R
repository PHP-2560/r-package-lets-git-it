#20 YEAR REPAYMENT OPTION
private.total <- input$undergrad_private_debt + input$med_private_debt #need to make this a thing

federal.total <- input$undergrad_federal_debt + input$med_federal_debt #need this value

#first things first, if they have private loans, they will need to repay these in the 'standard' way 
#*********************INSERT THIS REPAYMENT************
#create stand.repay vector (payment per year)
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
percent.payment <- gross*0.1
forgiven.amount <- income.driven(federal.total, input$avg_interest_rate, income=gross) 
prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, input$growth_rate, input$attending_tax, print.saved=0)
prep.payment <- vector(length = 20)
  for(i in 1:res){
   prep.payment[i]=0 
  }
  for(k in (res+1):20){
    prep.payment[k]=prep.payment.amount
  }
  
fed.remaining <- income.driven.vector(federal.total, input$avg_interest_rate, income=gross) #this gives how much of the fed debt is present each year


forgiveness.fund.annual_twenty<- vector(length = 20)
forgiveness.fund.annual[1:res] <- 0
for(i in (res+1):20) {
  forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i-1]  + prep.payment.amount
  forgiveness.fund.annual_twenty[i] = forgiveness.fund.annual_twenty[i]*(1+input$growth_rate)
} #*this would be the vector of how much you have saved to pay off the final forgiven amount

#combine the two repayments (fed and private) below
payment_twenty <- vector(length = 20)
for(i in 1:20) {
 payment_twenty[i] <- percent.payment[i] + stand.repay[i] + prep.payment[i]
}
payment_twenty[20] <- payment_twenty[20] + forgiven.amount*input$attending_tax #on the final year you pay extra to the IRS
total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
disposable_twenty <-  vector(length = 20)
for (i in 1:res){
  disposable_twenty[i] <- gross[i]*(1-input$residency_tax)-payment_twenty[i]
}
for (k in (res+1):19){
  disposable_twenty[k] <- gross[k]*(1-input$attending_tax)-payment_twenty[k]
}
disposable_twenty[20] <- gross[20]*(1-input$attending_tax)-stand.repay[20]-percent.payment[20]-prep.payment[20]-(forgiven.amount*input$attending_tax - forgiveness.fund.annual_twenty[20]) #on the last year, you also need to cover the gap between your prep fund and your IRS bill
cum_disposable_twenty <- cumsum(disposable_twenty)

debt_left_twenty <- priv.remaining + fed.remaining 

twenty_year_frame <- data.frame(years, gross, disposable_twenty, cum_disposable_twenty, debt_left_twenty, payment_twenty, total_paid_twenty, forgiveness.fund.annual_twenty)