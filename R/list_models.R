#' List available built-in PK models
#'
#' Displays the curated pharmacokinetic models available within the package.
#'
#' @param drug Optional drug name filter.
#' @param author Optional author name filter.
#'
#' @return A data frame with available models metadata.
#' @export
#'
#' @examples
#' list_models()
#' list_models(drug = "Tacrolimus")
#' list_models(author = "Han")
list_models <- function(drug = NULL, author = NULL) {

  models <- exeval_models

  if (!is.null(drug)) {
    models <- dplyr::filter(models, Drug == drug)
  }

  if (!is.null(author)) {
    models <- dplyr::filter(models, Author == author)
  }

  models %>%
    dplyr::select(
      Label,
      Drug,
      Author,
      Year,
      Ref
    )
}


