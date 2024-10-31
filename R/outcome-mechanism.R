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
