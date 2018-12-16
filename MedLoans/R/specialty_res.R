#' A Length of Residency Function
#'
#' This function returns the minimum length of residency for a given specialty.
#' @param specialty The specialty of interest.
#' @keywords residency, specialty
#' @export
#' @examples
#' specialty_res("Internal Medicine")

specialty_res <- function(specialty) {
  if(specialty == "Anesthesiology") {res = 4}
  if(specialty == "Dermatology") {res = 4}
  if(specialty == "Emergency Medicine") {res = 3}
  if(specialty == "Family Practice") {res = 3}
  if(specialty == "General Surgery") {res = 5}
  if(specialty == "Internal Medicine") {res = 3}
  if(specialty == "Neurology") {res = 4}
  if(specialty == "Neurosurgery") {res = 7}
  if(specialty == "Obstetrics/Gynecology") {res = 4}
  if(specialty == "Ophthalmology") {res = 4}
  if(specialty == "Orthopedic Surgery") {res = 5}
  if(specialty == "Otolaryngology") {res = 5}
  if(specialty == "Pathology") {res = 4}
  if(specialty == "Pediatrics") {res = 3}
  if(specialty == "Physical Medicine") {res = 4}
  if(specialty == "Plastic Surgery") {res = 6}
  if(specialty == "Psychiatry") {res = 4}
  if(specialty == "Radiation Oncology") {res = 5}
  if(specialty == "Radiology, Diagnostic") {res = 5}
  if(specialty == "Urology") {res = 5}
  if(specialty == "Cardiology") {res = 6}
  if(specialty == "Endocrinology") {res = 5}
  if(specialty == "Gastroenterology") {res = 6}
  if(specialty == "Infectious Disease") {res = 5}
  if(specialty == "Nephrology") {res = 5}
  if(specialty == "Oncology") {res = 5}
  if(specialty == "Pulmonary") {res = 5}
  if(specialty == "Rheumatology") {res = 5}
  if(specialty == "Immunology") {res = 5}
  print(res)
}