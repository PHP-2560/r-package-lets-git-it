interest_rate_avg <- .075
gross <- c(65000, 65000, 65000, 65000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000)
attending_tax <- 0.35
residency_tax <- 0.25
growth_rate <-.05
res <- 4
years <- c(1:20)
#just initializing values above for debugging below

#STARTING WITH Underserved:

#FOR PRIVATE DEBT

total.debt.underserved <- undergrad_private_debt + undergrad_federal_debt 
#this is the UG debt total
total.debt.underserved <- 100000 #initializing a value





#first things first, if they have private loans, they will need to repay these in the 'standard' way 
#*********************INSERT THIS REPAYMENT************
#create stand.repay vector (payment per year)
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
    underserved.remaining[k] <- underserved.remaining[k]*(1+interest_rate_avg)
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


