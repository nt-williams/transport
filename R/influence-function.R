influence_function <- function(x, ...) {
  UseMethod("influence_function")
}

#' @export
influence_function.standard <- function(x, task, ...) {
  task$reset()

  nuisance <- get_item(x, "pred")
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
  ipw_a <- pop / (1 - mean(pop)) * (a / nuisance$propensity[, "1"] - (1 - a) / (1 - nuisance$propensity[, "1"]))
  hs <- (1 - nuisance$population) / nuisance$population

  eic <- ipw_a * hs %*0% (y - nuisance$outcome[, "a"]) + ipw_target * (ord - lambda)

  ife::ife(lambda + mean(eic), eic)
}

#' @export
influence_function.ate <- function(x, task, ...) {
  task$reset()

  nuisance <- get_item(x, "pred")

  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  # ord: outcome regression difference
  ord <- nuisance$outcome[, "1"] - nuisance$outcome[, "0"]
  # propensity score
  pred_a <- a * nuisance$propensity[, "1"] + (1 - nuisance$propensity[, "1"]) * (1 - a)
  ipw <- (2*a - 1) / pred_a

  eic <- ipw %*0% (y - nuisance$outcome[, "a"]) + ord
  ife::ife(mean(eic), eic)
}

#' @export
influence_function.collaborative <- function(x, task, ...) {
  task$reset()

  nuisance <- get_item(x, "pred")

  pop <- task$select(task$col_roles$S)$data()
  a <- task$select(task$col_roles$A)$data()
  y <- task$select(task$col_roles$Y)$data()

  ipw_s1_marginal <- pop / (1 - mean(pop))
  ipw_s0_marginal <- (1 - pop) / (1 - mean(pop))

  pred_a <- a * nuisance$propensity[, "1"] + (1 - nuisance$propensity[, "1"]) * (1 - a)
  ipw <- (2*a - 1) / pred_a

  h_odds <- (1 - nuisance$hodds) / nuisance$hodds

  lambda <- mean(nuisance$heterogeneity[pop == 0])

  eic <-
    ipw_s1_marginal * ipw * h_odds %*0% (y - a*nuisance$cate - nuisance$outcome[, "0"]) +
    (1 - nuisance$population) / (1 - mean(pop)) * (nuisance$cate - nuisance$heterogeneity) +
    ipw_s0_marginal * (nuisance$heterogeneity - lambda)

  ife::ife(lambda + mean(eic), eic)
}

#' @export
influence_function.ittate <- function(x, task, ...) {
  task$reset()

  nuisance <- get_item(x, "pred")

  q <- nuisance$outcome
  q_iterated <- nuisance$iterated
  prob_a <- nuisance$propensity
  prob_s <- nuisance$population
  gz_ratio <- nuisance$ptc

  S <- task$select(task$col_roles$S)$data()
  A <- task$select(task$col_roles$A)$data()
  Y <- task$select(task$col_roles$Y)$data()

  marg_S1 <- mean(S)
  marg_S0 <- 1 - marg_S1

  pws_ratio <- (1 - prob_s) * marg_S1 / (prob_s * marg_S0)

  Ia1 <- I(A == 1)
  Ia0 <- I(A == 0)
  Is1 <- I(S == 1)
  Is0 <- I(S == 0)

  # Do A = 1
  psi1 <- mean(q_iterated[Is0, "1"])
  cY <- (Is1 * Ia1) / (prob_a[, "1"]*marg_S1) * gz_ratio[, "1"] * pws_ratio
  cZ <- (Is0 * Ia1) / (prob_a[, "0"] * marg_S0)
  cW <- Is0 / marg_S0

  eif_1 <- cY %*0% (Y - q[, "1"]) +
    cZ*(q[, "1"] - q_iterated[, "1"]) +
    cW*(q_iterated[, "1"] - psi1)

  # Do A = 0
  psi0 <- mean(q_iterated[Is0, "0"])
  cY <- (Is1 * Ia0) / ((1 - prob_a[, "1"])*marg_S1) * gz_ratio[, "0"] * pws_ratio
  cZ <- (Is0 * Ia0) / ((1 - prob_a[, "0"]) * marg_S0)
  cW <- Is0 / marg_S0

  eif_0 <- cY %*0% (Y - q[, "0"]) +
    cZ*(q[, "0"] - q_iterated[, "0"]) +
    cW*(q_iterated[, "0"] - psi0)

  ife::ife(psi1, eif_1) - ife::ife(psi0, eif_0)
}
