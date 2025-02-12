estimate_propensity.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  probs <- matrix(nrow = valid$nrow(), ncol = 2)
  colnames(probs) <- c("1", "0")

  features <- setdiff(train$features("A"), train$col_roles$S)
  target <- train$col_roles$A

  fit_1 <- train(
    train$pop("source")$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  probs[, "1"] <- predict(fit_1, newdata = valid$data())

  fit_0 <- train(
    train$pop("target")$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  probs[, "0"] <- predict(fit_0, newdata = valid$data())

  list(pred = probs,
       fit = list(S1 = return_full_fit(fit_1, control),
                  S0 = return_full_fit(fit_0, control)))
}
