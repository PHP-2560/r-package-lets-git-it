#' A Specialty Salary Function
#'
#' This function returns the average attending salary for a given specialty.
#' @param specialty The specialty of interest.
#' @keywords residency, specialty, salary
#' @export
#' @examples
#' specialty_salary()

specialty_salary <- function(specialty) {
  if(specialty == "Anesthesiology") {salary = 386000}
  if(specialty == "Dermatology") {salary = 392000}
  if(specialty == "Emergency Medicine") {salary = 350000}
  if(specialty == "Family Practice") {salary = 219000}
  if(specialty == "General Surgery") {salary = 322000}
  if(specialty == "Internal Medicine") {salary = 230000}
  if(specialty == "Neurology") {salary = 244000}
  if(specialty == "Neurosurgery") {salary = 663000}
  if(specialty == "Obstetrics/Gynecology") {salary = 300000}
  if(specialty == "Ophthalmology") {salary = 357000}
  if(specialty == "Orthopedic Surgery") {salary = 497000}
  if(specialty == "Otolaryngology") {salary = 383000}
  if(specialty == "Pathology") {salary = 286000}
  if(specialty == "Pediatrics") {salary = 212000}
  if(specialty == "Physical Medicine") {salary = 269000}
  if(specialty == "Plastic Surgery") {salary = 501000}
  if(specialty == "Psychiatry") {salary = 273000}
  if(specialty == "Radiation Oncology") {salary = 468000}
  if(specialty == "Radiology, Diagnostic") {salary = 401000}
  if(specialty == "Urology") {salary = 373000}
  if(specialty == "Cardiology") {salary = 423000}
  if(specialty == "Endocrinology") {salary = 212000}
  if(specialty == "Gastroenterology") {salary = 408000}
  if(specialty == "Infectious Disease") {salary = 231000}
  if(specialty == "Nephrology") {salary = 294000}
  if(specialty == "Oncology") {salary = 363000}
  if(specialty == "Pulmonary") {salary = 321000}
  if(specialty == "Rheumatology") {salary = 257000}
  if(specialty == "Immunology") {salary = 272000}
  print(salary)
}