#' A Loan Forgiveness Function
#'
#' This function calculates how much of your loan is forgiven.
#' @param base Starting value of the loan at time 0.
#' @param interest Interest rate on your loan.
#' @param income Income.
#' @param plan The timeframe you want to pay off your debt in. Default is 20.
#' @param print.saved Prints saved amount. Default is 1.
#' @keywords interest, income
#' @export
#' @examples
#' income <- c(65000,65000,65000,65000,65000, 353000, 353000,353000, 353000,353000, 353000,353000, 353000,353000, 353000,353000, 353000,353000, 353000,353000)
#' income.driven(400000,.075,income, plan=20)

income.driven <- function(base, interest, income, plan = 20) {
  if(!(base>0)) {warning("Did you provide a positive value for base?")}
  if(interest < -1 || interest>1) {interest = interest/100}
  if(length(income)<plan) {warning("Did you provide a vector of values for each year for income?")}
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