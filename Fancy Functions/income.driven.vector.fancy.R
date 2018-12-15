income.driven.vector1 <- function(base, interest,income, plan = 20) { #income needs to be a 20 element vector
  if(!(base>0)) {warning("Did you provide a positive value for base?")}
  if(interest < -1 || interest>1) {interest = interest/100}
  if(length(income)<plan) {warning("Did you provide a vector of values for each year for income?")}
  
  
  payment <- income/10 
  annual.debt <- vector(length=plan)
  annual.debt[plan] <- 0
  for(i in 1:plan){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) break
  }
  forgiven.amount <- annual.debt #whole vector, not just last value on this one
  print(forgiven.amount) 
}