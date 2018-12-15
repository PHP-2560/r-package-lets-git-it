grow1 <- function(base, interest, n=1) { #grow takes the base value and grows or shrinks it by the interest rate (which should be .07 if the rate is 7%), n is how many times it should compound
  if(!(base>0)) {warning("Did you provide a positive value for base?")}
  if(interest < -1 || interest>1) {interest = interest/100} 
  
  for(i in 1:n){#grows it by the rate for each iteration (n many iterations)
    base = base*(1+interest)
  }
  return(base)
}
