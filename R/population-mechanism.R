estimate_population.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  features <- train$features("S")
  target <- train$col_roles$S

  fit <- train(
    train$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  list(pred = matrix(predict(fit, newdata = valid$data()), ncol = 1),
       fit = return_full_fit(fit, control))
}
