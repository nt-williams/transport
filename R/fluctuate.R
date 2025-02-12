fluctuate_ittate <- function(task, param, nuisance) {
  switch(param,
         cy = fluctuate_ittate_y(task, nuisance),
         cz = fluctuate_ittate_z(task, nuisance)
  )
}

fluctuate_ittate_y <- function(task, nuisance) {
  gz_ratio <- nuisance$ptc$pred
  prob_a <- nuisance$propensity$pred[, "1"]
  prob_s <- nuisance$population$pred
  q <- nuisance$outcome$pred

  S <- task$select(task$col_roles$S)$data()
  A <- task$select(task$col_roles$A)$data()
  Y <- task$select(task$col_roles$Y)$data()

  marg_S1 <- mean(S)
  marg_S0 <- 1 - marg_S1

  pws_ratio <- (1 - prob_s) * marg_S1 / (prob_s * marg_S0)

  Ia1 <- I(A == 1)
  Ia0 <- I(A == 0)
  Is1 <- I(S == 1)

  # Do A = 1
  cY <- (Is1 * Ia1) / (prob_a*marg_S1) * gz_ratio[, "1"] * pws_ratio
  eps1 <- coef(glm(Y ~ -1 + offset(qlogis(q[, "a"])) + cY, family = "binomial"))

  q1_eps <- plogis(qlogis(q[, "1"]) + cY*eps1)

  # Do A = 0
  cY <- (Is1 * Ia0) / ((1 - prob_a)*marg_S1) * gz_ratio[, "0"] * pws_ratio
  eps0 <- coef(glm(Y ~ -1 + offset(qlogis(q[, "a"])) + cY, family = "binomial"))

  q0_eps <- plogis(qlogis(q[, "0"]) + cY*eps0)

  q_eps <- matrix(nrow = length(A), ncol = 2)
  colnames(q_eps) <- c("1", "0")
  q_eps[, "1"] <- q1_eps
  q_eps[, "0"] <- q0_eps

  list(preds = q_eps)
}

fluctuate_ittate_z <- function(task, nuisance) {
  prob_a <- nuisance$propensity$pred[, "0"]
  q <- nuisance$outcome$pred
  q_iterated <- nuisance$iterated$pred

  S <- task$select(task$col_roles$S)$data()
  A <- task$select(task$col_roles$A)$data()

  Ia1 <- I(A == 1)
  Ia0 <- I(A == 0)
  Is0 <- I(S == 0)

  marg_S0 <- 1 - mean(S)

  # Do A = 1
  cZ <- (Is0 * Ia1) / (prob_a * marg_S0)
  eps1 <- sw(coef(glm(q[, "1"] ~ -1 + offset(qlogis(q_iterated[, "1"])) + cZ, family = "binomial")))

  q1_iterated_eps <- plogis(qlogis(q_iterated[, "1"]) + cZ*eps1)

  # Do A = 0
  cZ <- (Is0 * Ia0) / ((1 - prob_a) * marg_S0)
  eps0 <- sw(coef(glm(q[, "0"] ~ -1 + offset(qlogis(q_iterated[, "0"])) + cZ, family = "binomial")))

  q0_iterated_eps <- plogis(qlogis(q_iterated[, "0"]) + cZ*eps0)

  q_iterated_eps <- matrix(nrow = length(A), ncol = 2)
  colnames(q_iterated_eps) <- c("1", "0")
  q_iterated_eps[, "1"] <- q1_iterated_eps
  q_iterated_eps[, "0"] <- q0_iterated_eps

  list(preds = q_iterated_eps)
}
