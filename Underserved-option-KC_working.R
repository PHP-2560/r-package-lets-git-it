total.debt.underserved <- input$undergrad_private_debt + input$undergrad_federal_debt 

underserved.payment <- pay.per.year(total.debt.underserved, input$avg_interest_rate)

underserved.repay <- vector(length=20)
  for(i in 1:res) {underserved.repay[i]=0}
  for(k in (res+1):20) {
    underserved.repay[k] <- underserved.payment
    }

underserved.remaining <- vector(length=20)
  for(i in 1:res) {
    underserved.remaining[i] <- grow(total.debt.underserved, input$avg_interest_rate, n=i)
  }
  for(k in (res+1):20) {
    underserved.remaining[k] <- (underserved.remaining[k-1] - underserved.payment)
    underserved.remaining[k] <- underserved.remaining[k]*(1+input$avg_interest_rate)
  }

#need disposable
disposable_underserved <-  vector(length = 20)
for (i in 1:res){
  disposable_underserved[i] <- gross[i]*(1-input$residency_tax)
}
for (k in (res+1):20){
  disposable_underserved[k] <- gross[k]*(1-input$attending_tax)-underserved.repay[k]
}

#need cumDisposable 
cum_disposable_underserved <- cumsum(disposable_underserved)

#debt_left is "underserved.remaining"
#payment_underserved is "underserved.repay"
#need cumUnderservedrepay
total_paid_underserved <- cumsum(underserved.repay)

underserved_frame <- data.frame(years, gross, disposable_underserved, cum_disposable_underserved, underserved.remaining, underserved.repay, total_paid_underserved)
underserved_frame
