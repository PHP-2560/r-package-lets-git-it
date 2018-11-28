payments <- function(base, interest, payment){ #annual payment made is payment, base is the debt, interest is the rate at which the debt grows
  if(interest < -1 | interest > 1) { #in case someone misenters interest as whole number instead of decimal form
    interest = interest/100
  }
  annual.debt <- numeric() #initialize the annual.debt vector
  for(i in 1:100){
    base <- (base-payment)*(1+interest) 
    annual.debt <- c(annual.debt, base)
    n<-i
    if(base<payment) break #break when remaining amount left to be paid is less than our payment amount
  }
  if (annual.debt[n] <= payment) {
    annual.debt <- c(annual.debt, 0)
  } 
  total.pay = n*payment + annual.debt[n] #in addition to making the consistent monthly payments, we need to add on the final payment which is less than the value we pay annually
  print(n+1) #print the number of payments we made
  print(total.pay) #print the total payments we made
  print(annual.debt) #print out the debt remaining each year
}

test <- payments(100, .07, 20) #testing our function
test


## Plotting payments
library(ggplot2)
library(ggthemes)
ggplot(as.data.frame(test), aes(1:length(test), test)) + geom_point() + geom_smooth(method = "lm") + xlab

graph_scatter <- function(input_vector, input_x, input_y, input_title) {
  ggplot(as.data.frame(input_vector), aes(1:length(input_vector), input_vector)) + geom_smooth() + geom_point() + xlab(input_x) + ylab(input_y) + ggtitle(input_title) + theme_economist() + scale_color_economist()
}

graph_scatter(test, "Number of Years", "Amount of Debt Remaining (in USD)", "Repayment Plan")
