train <- function(data, y, learners, outcome_type, id, folds, discrete, info) {
  fit <- mlr3superlearner::mlr3superlearner(
    data = data,
    target = y,
    library = learners,
    outcome_type = outcome_type,
    folds = folds,
    group = id,
    discrete = discrete,
    info = info
  )
  class(fit) <- append("nuisance", class(fit))
  fit
}

#' @export
predict.nuisance <- function(object, newdata, tol = .Machine$double.eps, ...) {
  pred <- NextMethod("predict", newdata = newdata)
  if (object$outcome_type == "continuous") return(pred)
  bound(pred, tol)
}
