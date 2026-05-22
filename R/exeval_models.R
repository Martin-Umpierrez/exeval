#' Built-in population PK/PKPD models
#'
#' Curated population pharmacokinetic (PK) and pharmacokinetic-pharmacodynamic
#' (PKPD) models included in the package for external evaluation workflows.
#'
#' These models can be used directly in [exeval_ppk()] by supplying the
#' corresponding \code{Label} as the \code{model} argument.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{Label}{Unique model identifier used to reference the model within
#'   the package.}
#'
#'   \item{Drug}{Drug associated with the model.}
#'
#'   \item{Author}{First author of the original publication.}
#'
#'   \item{Year}{Publication year.}
#'
#'   \item{Ref}{Reference title or citation for the original model publication.}
#'
#'   \item{Model_code}{Model code stored as a character string in
#'   \pkg{mrgsolve} format.}
#' }
#'
"exeval_models"

