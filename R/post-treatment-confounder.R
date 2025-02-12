estimate_ptc.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  dens_ratios <- matrix(nrow = valid$nrow(), ncol = 2)
  colnames(dens_ratios) <- c("0", "1")

  features <- setdiff(train$features("Z"), train$col_roles$S)
  target <- train$col_roles$Z

  fit1 <- train(
    train$pop("source")$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  fit0 <- train(
    train$pop("target")$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  z <- valid$select("Z")$data()

  a1s1 <- predict(fit1, valid$modify("A", 1))
  a1s1 <- z*a1s1 + (1-z)*(1-a1s1)
  a0s1 <- predict(fit1, valid$modify("A", 0))
  a0s1 <- z*a0s1 + (1-z)*(1-a0s1)
  a1s0 <- predict(fit0, valid$modify("A", 1))
  a1s0 <- z*a1s0 + (1-z)*(1-a1s0)
  a0s0 <- predict(fit0, valid$modify("A", 0))
  a0s0 <- z*a0s0 + (1-z)*(1-a0s0)

  dens_ratios[, "0"] <- a0s0 / a0s1
  dens_ratios[, "1"] <- a1s0 / a1s1

  list(pred = dens_ratios,
       fit1 = return_full_fit(fit1, control),
       fit0 = return_full_fit(fit0, control))
}
