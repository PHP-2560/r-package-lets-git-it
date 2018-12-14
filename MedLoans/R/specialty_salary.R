#' A Specialty Salary Function
#'
#' This function returns the average attending salary for a given specialty.
#' @param specialty The specialty of interest.
#' @keywords residency, specialty, salary
#' @export
#' @examples
#' specialty_salary()

specialty_salary <- function(specialty) {
  salary <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Annual Salary`)
  print(as.numeric(salary))
}