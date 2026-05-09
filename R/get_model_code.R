#' Get PK model code
#'
#' @param name Model name (e.g. "Han_etal_2011")
#' @export
#'
get_model_code <- function(name) {

  path <- system.file(
    "model_examples",
    paste0(name, ".R"),
    package = "preDose"
  )

  if (path == "") {
    stop("Model not found: ", name, call. = FALSE)
  }

  env <- new.env()
  source(path, local = env)

  if (!exists("model", env)) {
    stop("Model file must create an object called `model`", call. = FALSE)
  }

  env$model
}
