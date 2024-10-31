influence_function <- function(x, ...) {
  UseMethod("influence_function")
}

#' @export
influence_function.standard <- function(nuisance, task) {
  task$reset()

  nuisance <- lapply(nuisance, \(x) x[["probs"]])
  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  y[is.na(y)] <- -999

  # ord: outcome regression difference
  ord <- nuisance$outcome[, "1"] - nuisance$outcome[, "0"]
  lambda <- mean(ord[pop == 0])

  # Pr(A = a | H_a)
  pred_a <- a * nuisance$propensity + (1 - nuisance$propensity) * (1 - a)

  ipw_target <- (1 - pop) / (1 - mean(pop))
  ipw_source <- pop / mean(pop)
  ipw_a <- pop / (1 - mean(pop)) * (a / nuisance$propensity - (1 - a) / (1 - nuisance$propensity))
  hs <- (1 - nuisance$population) / nuisance$population

  eic <- ipw_a * hs * (y - nuisance$outcome[, "a"]) + ipw_target * (ord - lambda)

  task$pop("source")

  ipw <- pop / (1 - mean(pop)) * (1 / nuisance$propensity) * hs
  d.w <- survey::svydesign(~ 1, weights = ipw[pop == 1], data = task$data(reset = FALSE))
  f <- reformulate(task$col_roles$A, task$col_roles$Y)
  fit <- survey::svyglm(f, design = d.w, data = task$data())

  ife::ife(lambda + mean(eic), eic)
}
