transport_ate <- function(data, trt, outcome, covar, pop,
                          obs = NULL, id = NULL, weights = NULL,
                          estimator = c("standard", "collaborative"),
                          learners_trt = "glm",
                          learners_pop = "glm",
                          learners_outcome = "glm",
                          folds = 1, control = transport_control()) {

  task <- as_transport_task(
    data = data,
    A = trt,
    Y = outcome,
    W = covar,
    S = pop,
    C = obs,
    id = id,
    weights = weights,
    folds = folds
  )

  # the number likely needs to be dynamic
  pb <- progressr::progressor(folds*3)

  nuisance <- structure(
    list(
      propensity = crossfit_propensity(task, learners_trt, control, pb),
      population = crossfit_population(task, learners_pop, control, pb),
      outcome = crossfit_outcome(task, learners_outcome, control, pb)
    ),
    class = match.arg(estimator)
  )

  psi <- influence_function(nuisance, task)

  structure(
    list(
      psi = psi,
      nuisance = lapply(nuisance, \(x) x[["probs"]]),
      fits = lapply(nuisance, \(x) x[["fits"]]),
      call = match.call()
    ),
    class = "transported_ate"
  )
}
