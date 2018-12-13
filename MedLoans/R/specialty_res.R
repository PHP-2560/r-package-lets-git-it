specialty_res <- function(specialty) {
  res <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Years of Training`)
  print(as.numeric(res))
}