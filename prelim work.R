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

#We obtained average physician salaries from Medscape and Doximity.
#neurosurgery and radiation oncology came from doximity, everything else came from Medscape
"https://residency.wustl.edu/residencies/length-of-residencies/" #This is where we obtained residency lengths from.
"https://www.uwmedicine.org/education/Pages/specialties-subspecialties.aspx" #additional training lenghts

##working with specialty_info.csv
library(readr)
library(scales)
library(ggrepel)
specialty_info <- read_csv("specialty_info.csv")
View(specialty_info)

p <- ggplot(specialty_info, aes(`Years of Training`, `Annual Salary`)) + geom_smooth(method = "lm") + geom_point() + scale_y_continuous(labels = comma) + xlab("Years of Training") + ylab("Average Annual Salary (in USD)") + ggtitle("Average Annual Earnings for Each Specialty") + theme_economist() + scale_color_economist()
p + geom_label_repel(aes(label = Specialty),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50',
                     size = 2)
wow <- lm(`Annual Salary`~`Years of Training`, data = specialty_info)
summary(wow)

#assigning specialty characteristics if statements
if(specialty == "Anesthesiology") {res = 4& salary = 386000}
if(specialty == "Dermatology") {res = 4& salary = 392000}
if(specialty == "Emergency Medicine") {res = 3& salary = 350000}
if(specialty == "Family Practice") {res = 3& salary = 219000}
if(specialty == "General Surgery") {res = 5& salary = 322000}
if(specialty == "Internal Medicine") {res = 3& salary = 230000}
if(specialty == "Neurology") {res = 4& salary = 244000}
if(specialty == "Neurosurgery") {res = 7& salary = 663000}
if(specialty == "Obstetrics/Gynecology") {res = 4& salary = 300000}
if(specialty == "Ophthalmology") {res = 4& salary = 357000}
if(specialty == "Orthopedic Surgery") {res = 5& salary = 497000}
if(specialty == "Otolaryngology") {res = 5& salary = 383000}
if(specialty == "Pathology") {res = 4& salary = 286000}
if(specialty == "Pediatrics") {res = 3& salary = 212000}
if(specialty == "Physical Medicine") {res = 4& salary = 269000}
if(specialty == "Plastic Surgery") {res = 6& salary = 501000}
if(specialty == "Psychiatry") {res = 4& salary = 273000}
if(specialty == "Radiation Oncology") {res = 5& salary = 468000}
if(specialty == "Radiology, Diagnostic") {res = 5& salary = 401000}
if(specialty == "Urology") {res = 5& salary = 373000}
if(specialty == "Cardiology") {res = 6& salary = 423000}
if(specialty == "Endocrinology") {res = 5& salary = 212000}
if(specialty == "Gastroenterology") {res = 6& salary = 408000}
if(specialty == "Infectious Disease") {res = 5& salary = 231000}
if(specialty == "Nephrology") {res = 5& salary = 294000}
if(specialty == "Oncology") {res = 5& salary = 363000}
if(specialty == "Pulmonary") {res = 5& salary = 321000}
if(specialty == "Rheumatology") {res = 5& salary = 257000}
if(specialty == "Immunology") {res = 5& salary = 272000}

res = res + fellowship