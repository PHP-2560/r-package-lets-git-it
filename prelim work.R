grow <- function(base, interest, n = 1) {
  
  
}

names <- vector(length = 5)
names




payments <- function(base, interest, payment){ #annual payment made is payment, base is the debt, interest is the rate at which the debt grows
  if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
    interest = interest/100
  }
  annual.debt <- vector(length=100) #initialize the annual.debt vector
  for(i in 1:100){
    base = (base-payment)*(1+interest)
    annual.debt[i] <- base
    n<-i
    if(base<payment) break #break when remaining amount left to be paid is less than our payment amount
  }
  total.pay = n*payment + annual.debt[n] #in addition to making the consistent monthly payments, we need to add on the final payment which is less than the value we pay monthly
  print(annual.debt) #print out the debt remaining each year
  print(n+1) #print the number of payments we made
  print(total.pay) #print the total payments we made
}

payments(100, .07, 10)

