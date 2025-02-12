estimate_outcome.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  probs <- matrix(nrow = valid$nrow(), ncol = 3)
  colnames(probs) <- c("a", "1", "0")

  features <- train$features("Y")
  target <- train$col_roles$Y

  train$pop("source")$obs()$select(c(features, target))

  fit <- train(
    train$pop("source")$obs()$select(c(features, target))$data(),
    target,
    learners,
    train$outcome_type,
    train$pop("source")$obs()$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  probs[, "a"] <- predict(fit, valid$data())
  probs[, "1"] <- predict(fit, valid$modify("A", 1))
  probs[, "0"] <- predict(fit, valid$modify("A", 0))

  list(pred = probs,
       fit = return_full_fit(fit, control))
}

estimate_itt_iterated.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  probs <- matrix(nrow = valid$nrow(), ncol = 2)
  colnames(probs) <- c("1", "0")

  features <- train$col_roles$W
  A <- train$col_roles$A
  tmp <- train$
    pop("target")$
    obs()$
    select(c(features, ".outcome_eps1", ".outcome_eps0", A))$
    data()

  # do A = 1
  fit_1 <- train(
    tmp[tmp[[A]] == 1, c(features, ".outcome_eps1")],
    ".outcome_eps1",
    learners,
    "continuous",
    train$pop("target")$obs()$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  # do A = 0
  fit_0 <- train(
    tmp[tmp[[A]] == 0, c(features, ".outcome_eps1")],
    ".outcome_eps1",
    learners,
    "continuous",
    train$pop("target")$obs()$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  probs[, "1"] <- predict(fit_1, valid$data())
  probs[, "0"] <- predict(fit_0, valid$data())

  list(pred = probs,
       fit_1 = return_full_fit(fit_1, control),
       fit_0 = return_full_fit(fit_0, control))
}
