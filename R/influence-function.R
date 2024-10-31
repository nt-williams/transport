influence_function <- function(x, ...) {
  UseMethod("influence_function")
}

#' @export
influence_function.standard <- function(nuisance, task) {
  task$reset()

  nuisance <- get_item(nuisance, "pred")
  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  # ord: outcome regression difference
  ord <- nuisance$outcome[, "1"] - nuisance$outcome[, "0"]
  lambda <- mean(ord[pop == 0])

  # Pr(A = a | H_a)
  pred_a <- a * nuisance$propensity + (1 - nuisance$propensity) * (1 - a)

  ipw_target <- (1 - pop) / (1 - mean(pop))
  ipw_source <- pop / mean(pop)
  ipw_a <- pop / (1 - mean(pop)) * (a / nuisance$propensity - (1 - a) / (1 - nuisance$propensity))
  hs <- (1 - nuisance$population) / nuisance$population

  eic <- ipw_a * hs %*0% (y - nuisance$outcome[, "a"]) + ipw_target * (ord - lambda)

  task$pop("source")

  ipw <- pop / (1 - mean(pop)) * (1 / nuisance$propensity) * hs
  d.w <- survey::svydesign(~ 1, weights = ipw[pop == 1], data = task$data(reset = FALSE))
  f <- reformulate(task$col_roles$A, task$col_roles$Y)
  fit <- survey::svyglm(f, design = d.w, data = task$data())

  ife::ife(lambda + mean(eic), eic)
}

influence_function.ate <- function(nuisance, task) {
  task$reset()

  nuisance <- get_item(nuisance, "pred")

  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  # ord: outcome regression difference
  ord <- nuisance$outcome[, "1"] - nuisance$outcome[, "0"]
  # propensity score
  pred_a <- a * nuisance$propensity + (1 - nuisance$propensity) * (1 - a)
  ipw <- (2*a - 1) / pred_a

  eic <- ipw %*0% (y - nuisance$outcome[, "a"]) + ord
  ife::ife(mean(eic), eic)
}

influence_function.collaborative <- function(nuisance, task) {
  task$reset()

  nuisance <- get_item(nuisance, "pred")

  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  ipw_s1_marginal <- pop / (1 - mean(pop))
  ipw_s0_marginal <- (1 - pop) / (1 - mean(pop))

  pred_a <- a * nuisance$propensity + (1 - nuisance$propensity) * (1 - a)
  ipw <- (2*a - 1) / pred_a

  h_odds <- (1 - nuisance$hodds) / nuisance$hodds

  lambda <- mean(nuisance$heterogeneity[pop == 0])

  eic <-
    ipw_s1_marginal * ipw * h_odds %*0% (y - a*nuisance$cate - nuisance$outcome[, "0"]) +
    (1 - nuisance$population) / (1 - mean(pop)) * (nuisance$cate - nuisance$heterogeneity) +
    ipw_s0_marginal * (nuisance$heterogeneity - lambda)

  ife::ife(lambda + mean(eic), eic)
}
