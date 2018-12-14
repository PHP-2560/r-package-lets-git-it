#' A Vectorized Loan Forgiveness Function
#'
#' This function calculates how much of your loan is forgiven. Similar to the 
#' regular income.driven() function but returns a vector.
#' @param base Starting value of the loan at time 0.
#' @param interest Interest rate on your loan.
#' @param income Income. Needs to be a vector.
#' @param plan The timeframe you want to pay off your debt in. Default is 20.
#' @param print.saved Prints saved amount. Default is 1.
#' @keywords interest, income
#' @export
#' @examples
#' income.driven.vector()

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