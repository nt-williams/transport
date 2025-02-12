#' @importFrom cli cli_div cli_rule cli_end cli_h3
#' @export
print.transported_ate <- function(x, ...) {
    cat("\n")
    d <- cli_div(theme = list(rule = list("line-type" = "double")))
    cli_rule(left = "Results from {.fn transport_ate}")
    cat("\n")
    cli_end(d)
    print(x$psi)
}

#' @importFrom cli cli_div cli_rule cli_end cli_h3
#' @export
print.transported_ittate <- function(x, ...) {
  cat("\n")
  d <- cli_div(theme = list(rule = list("line-type" = "double")))
  cli_rule(left = "Results from {.fn transport_ittate}")
  cat("\n")
  cli_end(d)
  print(x$psi)
}
