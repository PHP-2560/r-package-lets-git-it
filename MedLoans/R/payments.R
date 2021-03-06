#' A Payments Function
#'
#' This function allows you to keep track of your payments toward your debt.
#' @param base Starting value of the loan at time 0.
#' @param interest Interest rate on the loan.
#' @param payment The annual payment you intend to make.
#' @keywords debt, interest
#' @export
#' @examples
#' payments(100000, .075, 10000)

payments <- function(base, interest, payment){ #annual payment made is payment, base is the debt, interest is the rate at which the debt grows
  if(!(base>0)) {warning("Did you provide a positive value for base?")}
  if(interest < -1 || interest>1) {interest = interest/100}
  if(!(payment>0)) {warning("Did you provide a positive annual payment?")}
  
  annual.debt <- numeric() #initialize the annual.debt vector
  for(i in 1:100){
    base <- (base-payment)*(1+interest) 
    annual.debt <- c(annual.debt, base)
    n<-i
    if(base<payment) break #break when remaining amount left to be paid is less than our payment amount
  }
  if(annual.debt[n]< payment) annual.debt <- c(annual.debt, 0)
  total.pay = n*payment + annual.debt[n] #in addition to making the consistent annual payments, we need to add on the final payment which is less than the value we pay annually
  print(n+1) #print the number of payments we made
  print(total.pay) #print the total payments we made
  print(annual.debt) #print out the debt remaining each year
  
}
