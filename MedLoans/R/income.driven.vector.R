income.driven.vector <- function(base, interest,income, plan = 20) { #income needs to be a 20 element vector
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