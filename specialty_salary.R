specialty_salary <- function(specialty) {
  salary <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Annual Salary`)
  print(as.numeric(salary))
}