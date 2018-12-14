#' A Length of Residency Function
#'
#' This function returns the minimum length of residency for a given specialty.
#' @param specialty The specialty of interest.
#' @keywords residency, specialty
#' @export
#' @examples
#' specialty_res()

specialty_res <- function(specialty) {
  res <- specialty_info %>%
    filter(Specialty == specialty) %>%
    select(`Years of Training`)
  print(as.numeric(res))
}