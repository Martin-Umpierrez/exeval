#' Tacrolimus pharmacokinetic data in kidney transplant patients
#'
#' Pharmacokinetic and clinical data from adult kidney transplant recipients
#' treated with tacrolimus, used for population pharmacokinetic model
#' development, external evaluation, and methodological package examples.
#'
#' This dataset corresponds to a Uruguayan kidney transplant cohort.
#' 
#' 
#' @format A data.frame with 739 rows and 30 variables:
#' \describe{
#'   \item{ID}{Patient identifier}
#'   \item{OCC}{Number of the occasion}
#'   \item{DD}{Tacrolimus daily dose (mg)}
#'   \item{AMT}{Dose amount (mg)}
#'   \item{TIME}{Sequential time (hours)}
#'   \item{POD}{Post-operative days}
#'   \item{DV}{Observed tacrolimus concentration (ng/mL)}
#'   \item{EVID}{Event identifier}
#'   \item{CMT}{Compartment identifier}
#'   \item{MDV}{Missing dependent variable flag}
#'   \item{II}{Dosing interval (hours)}
#'   \item{SS}{Steady-state indicator}
#'   \item{Creatine}{Creatinine (mg/dL)}
#'   \item{SCR}{Serum creatinine (\eqn{\mu mol/L})}
#'   \item{eGFR}{Estimated glomerular filtration rate (mL/min/1.73 m\eqn{^2})}
#'   \item{ClCrea}{Creatinine clearance (Cockcroft-Gault, mL/min)}
#'   \item{AGE}{Age (years)}
#'   \item{SEX}{Sex}
#'   \item{WT}{Body weight (kg)}
#'   \item{HCT}{Hematocrit}
#'   \item{CYP3A5}{CYP3A5 polymorphism}
#'   \item{EXPRESSION}{CYP3A5 expresser status}
#'   \item{PDN_DOSE}{Prednisone dose (mg)}
#'   \item{PDNXWT}{Prednisone dose normalized by body weight (mg/kg)}
#'   \item{Height}{Height (cm)}
#'   \item{Height..m.}{Height (m)}
#'   \item{BSA}{Body surface area (m\eqn{^2})}
#'   \item{BMIcalc}{Body mass index (kg/m\eqn{^2})}
#'   \item{LBW}{Lean body weight (kg)}
#'   \item{DMELITU}{Diabetes mellitus indicator}
#' }
#'
#' @references
#' Umpierrez M, et al. (2025).
#' \emph{Accelerating Tacrolimus Model-Informed Precision Dosing in Kidney
#' Transplant Recipients: Model Evaluation and Refinement Strategies.}
#'
#' @source
#' De-identified clinical dataset adapted for methodological research and
#' package examples.
"tacrolimus_pk1_kidney"