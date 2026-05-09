#' Get path to PK model file
#'
#' @param name Model name
#' @param ext File extension ('cpp' or 'R')
#' @export

get_model_path <-
function(name, ext = c("cpp", "R")) {
  ext <- match.arg(ext)

  path <- system.file(
    "model_examples",
    paste0(name, ".", ext),
    package = "preDose"
  )

  if (path == "") {
    stop("Model not found: ", name)
  }

  path
}
