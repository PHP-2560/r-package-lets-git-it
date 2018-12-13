income.driven <- function(base, interest, income, plan = 20) {
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