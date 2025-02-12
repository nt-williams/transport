transport_ittate <- function(data, instrument, trt, outcome, covar, pop,
                             obs = NULL, id = NULL, weights = NULL,
                             learners_trt = "glm",
                             learners_pop = "glm",
                             learners_outcome = "glm",
                             learners_ptc = "glm",
                             folds = 1,
                             control = transport_control()) {
  call <- match.call()

  task <- as_transport_task(
    data = data,
    A = instrument,
    Z = trt,
    Y = outcome,
    W = covar,
    S = pop,
    C = obs,
    id = id,
    weights = weights,
    folds = folds
  )

  nuisance <- list(
    propensity = crossfit(task, "propensity", learners_trt, control),
    population = crossfit(task, "population", learners_pop, control),
    outcome    = crossfit(task, "outcome", learners_outcome, control),
    ptc        = crossfit(task, "ptc", learners_ptc, control)
  )

  # Fluctuate D_y
  nuisance$outcome$pred <- fluctuate_ittate(task, "cy", nuisance)$pred

  nuisance$iterated <- crossfit(
    task$
      clone()$
      add_var(nuisance$outcome$pred[, "1"], ".outcome_eps1")$
      add_var(nuisance$outcome$pred[, "0"], ".outcome_eps0"),
    "itt_iterated",
    learners_outcome,
    control
  )

  # Fluctuate D_z
  nuisance$iterated$pred <- fluctuate_ittate(task, "cz", nuisance)$pred

  output(influence_function(structure(nuisance, class = "ittate"), task),
         nuisance,
         call,
         "transported_ittate")
}
