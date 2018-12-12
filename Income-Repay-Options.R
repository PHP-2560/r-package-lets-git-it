
#STARTING WITH 20 YEAR:

#FOR PRIVATE DEBT
private.total <- undergrad_private_debt + med_private_debt #need to make this a thing
private.total <- 100000 #initializing a value to work with

federal.total <- undergrad_federal_debt + med_federal_debt #need this value
federal.total <- 400000 #initializing a value to work with
#Next I will launch the needed function income.driven and forgiveness.prep.fund
income.driven <- function(base, interest,income) {
  payment <- income/10 #****ULTIMATELY WE SHOULD CHANGE INCOME TO BE A VECTOR, IT WILL VARY BY YEAR (ESP MOVING FROM RESIDENCY INTO ATTENDINGHOOD) - in the for loop we will then need to call the index year of income***
  annual.debt <- vector(length=20)
  annual.debt[20] <- 0
  for(i in 1:20){
    base = (base-payment)*(1+interest)
    annual.debt[i] <- base
    if(base<0) break
  }
  forgiven.amount <- annual.debt[20]
  print(forgiven.amount) #how much is forgiven
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
  print(test) #prints how much you need to save per year
  if(print.saved==1) {
    print(saved)
  }
} 














#first things first, if they have private loans, they will need to repay these in the 'standard' way 
#*********************INSERT THIS REPAYMENT************
#create stand.repay vector (payment per year)


stand.repay <- vector(length=20)
  for(i in 1:res) {stand.repay[i]=0}
  for(k in (res+1):20) {stand.repay[k] <- #NEED TO INSERT HOW MUCH TO PAY PER YEAR*****}

















#For the remaining federal debt they will be repaying 10% of their income for 20 years:
#gross should have already been called
percent.payment <- gross*0.1
forgiven.amount <- income.driven(federal.total, interest, sal)
prep.payment.amount <- forgiveness.prep.fund(forgiven.amount, growth_rate, attending_tax)
prep.payment <- vector(length = 20)
  for(i in 1:res){
   prep.payment[i]=0 
  }
  for(k in (res+1):20){
    prep.payment[k]=prep.payment.amount
  }


forgiveness.fund.annual #make the vector


forgiveness.fund.annual<- vector(length = 20)
forgiveness.fund.annual[1:ress] <- 0
for(i in (res+1):20) {
  forgiveness.fund.annual[i] = forgiveness.fund.annual[i-1]  + prep.payment.amount
  forgiveness.fund.annual[i] = forgiveness.fund.annual[i]*(1+growth_rate)
} #*this would be the vector of how much you have saved to pay off the final forgiven amount

























#combine the two repayments (fed and private) below
payment_twenty <- vector(length = 20)
for(i in 1:20) {
 payment_twenty[i] <- perecent.payment[i] + stand.repay[i] + prep.payment[i]
}
total_paid_twenty <- cumsum(payment_twenty) #cumulative payments
disposable_twenty <-  vector(length = 20)
for (i in 1:res){
  disposable_twenty[i] <- gross[i]*(1-residency_tax)-payment_twenty[i]
}
for (k in (res+1):20){
  disposable_twenty[k] <- gross[k]*(1-attending_tax)-payment_twenty[k]
}
cum_disposable_twenty <- cumsum(disposable_twenty)

debt_left_twenty #***********THIS ONE WILL BE A BIT COMPLEX
#***********May want to add a column to the df of the forgiveness repayment fund (i.e. the forgiveness.fund.annual vector)





standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)

ten_year_frame <- data.frame(years, gross)
twenty_year_frame <- data.frame(years, gross)
military_frame <- data.frame(years, gross)
underserved_frame <- data.frame(years, gross)