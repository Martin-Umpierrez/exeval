#' List available example PK models
#'
#' @export

list_models <-
function() {

  path <- system.file("model_examples", package = "preDose")

  files <- list.files(path)

  unique(gsub("\\.(R|cpp)$", "", files))
}
