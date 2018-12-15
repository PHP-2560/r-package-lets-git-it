income.driven1 <- function(base, interest, income, plan = 20) {
  if(!(base>0)) {warning("Did you provide a positive value for base?")}
  if(interest < -1 || interest>1) {interest = interest/100}
  if(!(income>0)) {warning("Did you provide a positive value for income?")}
  
  
  payment <- income/10 
  annual.debt <- vector(length=plan)
  annual.debt[plan] <- 0
  for(i in 1:plan){
    base = (base-payment[i])*(1+interest)
    annual.debt[i] <- base
    if(base<0) {break}
  }
  forgiven.amount <- annual.debt[10]
  print(forgiven.amount) #how much is forgiven
}