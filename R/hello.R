#' Say "Hello" to arg
#'
#' @param name the function says "Hello" to
#' @export

hello <- function(name) {
  print(paste("Hello, ", name, "!", sep = ""))
}
