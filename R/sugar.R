transport_task <- function(data, trt, outcome, covar, pop,
                           modifiers = NULL, obs = NULL,
                           group = NULL, weights = NULL, folds = 1) {
  TransportTask$new(data, trt, outcome, covar, pop, modifiers, obs, group, weights, folds)
}
