crossfit <- function(x, ...) {
  UseMethod("crossfit")
}

#' @export
crossfit.TransportTask <- function(x, param, learners, control, ...) {
  ans <- vector("list", length = x$nfolds())

  for (fold in seq_along(x$folds)) {
    train <- x$training(fold)
    valid <- x$validation(fold)

    ans[[fold]] <-
      # future::future({
      switch(param,
        propensity = estimate_propensity.TransportTask(train, valid, learners, control),
        population = estimate_population.TransportTask(train, valid, learners, control),
        outcome = estimate_outcome.TransportTask(train, valid, learners, control),
        cate = estimate_cate.TransportTask(train, valid, learners, control),
        hodds = estimate_hodds.TransportTask(train, valid, learners, control),
        heterogeneity = estimate_heterogeneity.TransportTask(train, valid, learners, control),
        ptc = estimate_ptc.TransportTask(train, valid, learners, control),
        itt_iterated = estimate_itt_iterated.TransportTask(train, valid, learners, control)
      )
    # },
    # seed = TRUE)
  }

  # ans <- future::value(ans)

  list(pred = recombine(rbind_depth(ans, "pred"), x$folds),
       fits = lapply(ans, \(x) x[["fit"]]))
}
