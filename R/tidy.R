#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.transported_ate <- function(x, ...) {
  ife::tidy(x$psi)
}
