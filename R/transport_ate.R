transport_ate <- function(data,
                          trt,
                          outcome,
                          covar,
                          pop,
                          modifiers = NULL,
                          obs = NULL,
                          id = NULL,
                          weights = NULL,
                          estimator = c("standard", "collaborative", "ral"),
                          learners_trt = "glm",
                          learners_pop = "glm",
                          learners_outcome = "glm",
                          folds = 1,
                          control = transport_control()) {

  task <- as_transport_task(
    data = data,
    A = trt,
    Y = outcome,
    W = covar,
    S = pop,
    C = obs,
    id = group,
    weights = weights,
    folds = folds
  )

  # the number likely needs to be dynamic
  pb <- progressr::progressor(folds*3)

  prop_scores <- crossfit_propensity(task, learners_trt, control, pb)

}
