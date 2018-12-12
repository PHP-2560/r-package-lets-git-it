res <- specialty_res("Psychiatry")


salary <- specialty_salary("Psychiatry")



grow(10,7,10)



pay.per.year(600000, .075, years=16) #this pay.per.year gives us a fixed number for the person to pay during each year of their attendinghood

payments(100, .07, 12)#testing our function

years <- c(1:20)
gross <- vector(length = 20)
for (i in 1:res) {
  gross[i] <- 65000
}
for (k in (res+1):20) {
  gross[k] <- salary
}
gross


debt_total <- total_federal_debt + total_private_debt

###TESTING
debt_total <- 300000
avg_interest_rate <- 0.076

debt_payment <- pay.per.year(debt_total, avg_interest_rate)
payments_output_standard <- payments(grow(debt_total, avg_interest_rate, n = res), avg_interest_rate, debt_payment)
debt_payment_standard <- vector(length = 20)
for (i in 1:res) {
  debt_payment_standard[i] <- 0
}
for (k in (res+1):20) {
  debt_payment_standard[k] <- debt_payment
}
debt_payment_standard
total_paid_standard <- cumsum(debt_payment_standard) #cumulative payments

debt_left_standard <- vector(length = 20)
debt_left_standard[1] <- grow(debt_total, avg_interest_rate)
for (i in 2:res) {
  debt_left_standard[i] <- grow(debt_left_standard[i-1], avg_interest_rate)
}
for (k in (res+1):20) {
  debt_left_standard[k] <- payments_output_standard[k-res]
}
debt_left_standard

disposable <- vector(length = 20)

disposable <- c(1, 2, 3, 4, 5, 6, 7)

residency_tax <- 0.25
attending_tax <- 0.35

disposable_standard <- vector(length = 20)
for (i in 1:res){
  disposable_standard[i] <- gross[i]*(1-residency_tax)-debt_payment_standard[i]
}
for (k in (res+1):20){
  disposable_standard[k] <- gross[k]*(1-attending_tax)-debt_payment_standard[k]
}
disposable_standard


cum_disposable_standard <- cumsum(disposable_standard)


standard_frame <- data.frame(years, gross, disposable_standard, cum_disposable_standard, debt_left_standard, debt_payment_standard, total_paid_standard)

ten_year_frame <- data.frame(years, gross)
twenty_year_frame <- data.frame(years, gross)
military_frame <- data.frame(years, gross)
underserved_frame <- data.frame(years, gross)