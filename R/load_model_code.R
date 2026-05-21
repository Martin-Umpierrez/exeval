#' Load built-in model code
#'
#' Retrieves population pharmacokinetic model code from the internal
#' curated model library included in the package.
#'
#' @param model_id Character string with the model identifier.
#'
#' @return A character string containing the model code.
#'
#'#' @details
#' The returned object is a character string compatible with
#' \code{mrgsolve::mcode()} and can be directly used in package functions
#' that accept model code as character input (e.g.
#' \code{exeval_ppk()} or \code{run_MAP_estimations()}).
#'
#' @export
#'
#' @examples
#' mod <- load_model_code("TAC_Han2011")
#'
#' # Run evaluation
#' # res <- exeval_ppk(
#' #   model = mod,
#' #   model_name = "TAC_Han2011",
#' #   data = my_data
#' # )
load_model_code <- function(label) {

  idx <- exeval_models$Label == label

  if (!any(idx)) {
    stop(
      "Model not found. Use list_models() to see available models."
    )
  }

  code <- exeval_models$Model_code[idx][[1]]

  code <- trimws(code)
  code <- sub('^"', "", code)
  code <- sub('"$', "", code)

  return(code)
}
