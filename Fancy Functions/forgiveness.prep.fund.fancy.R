forgiveness.prep.fund1 <- function(amount.forgiven, growth.rate, tax.rate, res, years =20, print.saved=1) {#determines how much you need to put away per year to cover your tax bill on the forgiven amount from income.driven or income.driven short (for income driven short you would need to change years to 10) --- I assume people aren't putting money toward this during residency
  if(!(amount.forgiven>0)) {warning("Did you provide a positive value for amount.forgiven?")}
  if(growth.rate < -1 || growth.rate>1) {growth.rate = growth.rate/100}
  if(growth.rate < -1 || tax.rate>1) {tax.rate = tax.rate/100}
  if(!(res>=0 && res<=12)) {warning("Did you provide a reasonable value for res length?")}

  
  years <- years-res
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
