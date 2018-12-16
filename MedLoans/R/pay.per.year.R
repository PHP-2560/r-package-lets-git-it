#' A Pay Per Year Function
#'
#' This function allows you calculate how much you need to pay per year
#' to pay off your loans after residency.
#' @param debt.total The total amount of debt you have remaining.
#' @param interest Interest rate on the loan.
#' @param res The length of your residency.
#' @param years The timeframe you want to pay off your debt in. Default is 20.
#' @keywords debt, interest
#' @export
#' @examples
#' pay.per.year(400000, .075, 3, years = 20)

pay.per.year <- function(debt.total, interest, res, years = 20) {
  if(!(debt.total>0)) {warning("Did you provide a positive value for debt.total?")}
  if(interest < -1 || interest>1) {interest = interest/100}
  if(!(res>=0 && res<=12)) {warning("Did you provide a reasonable value for res length?")}
  years = years - res
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