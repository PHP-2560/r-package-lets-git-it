#how much to pay per year:
#This will take the debt total grown after res and calculate how much to pay per year to pay off loan after res
pay.per.year <- function(debt.total, interest, years =20-res) {
  debt.total.grown = grow(debt.total, interest, n = res)
  testpay = debt.total*interest*3
  for(k in 1:20) {testpay <- testpay*0.95
  DT <- debt.total.grown
  for(i in 1:years) {
    DT <- DT - testpay
    DT <- DT*(1+interest)
  }
  if(length(payments(debt.total.grown, interest, testpay)) >= years){ break} #we are trying to stop it when the repayment plan finishes at the right time
  } 
  
  print(testpay) #prints how much you need to pay per year
}


grow(300000,.075,n=4)
res

pay.per.year(300000,.075)
payments(400640,.075, 40414)
