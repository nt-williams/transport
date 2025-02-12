get_outcome_type = function(data, var) {
  target <- na.omit(data[[var]])

  if (length(unique(target)) > 2) {
    return("continuous")
  }

  assert_binary(target)
  "binomial"
}

bound <- function(x, p = 1e-05) {
  pmax(pmin(x, 1 - p), p)
}

return_full_fit <- function(fit, control) {
  if (control$.return_full_fits) {
    return(fit)
  }
  summary(fit)
}

#' @export
summary.mlr3superlearner <- function(fit) {
  cbind(Risk = fit$risk)
}

`%*0%` <- function(x, y) {
  res <- x * y
  res[is.na(res)] <- 0
  res
}

get_item <- function(x, name) {
  lapply(x, \(y) y[[name]])
}

sw <- function(x) suppressWarnings(x)
