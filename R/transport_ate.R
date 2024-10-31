#' Title
#'
#' @param data
#' @param trt
#' @param outcome
#' @param covar
#' @param pop
#' @param obs
#' @param id
#' @param weights
#' @param estimator
#' @param learners_trt
#' @param learners_pop
#' @param learners_outcome
#' @param learners_heterogeneity
#' @param folds
#' @param control
#'
#' @return
#' @export
#'
#' @examples
transport_ate <- function(data, trt, outcome, covar, pop,
                          obs = NULL, id = NULL, weights = NULL,
                          estimator = c("standard", "collaborative"),
                          learners_trt = "glm",
                          learners_pop = "glm",
                          learners_outcome = "glm",
                          learners_heterogeneity = "glm",
                          folds = 1, control = transport_control()) {
  call <- match.call()

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

  nuisance <- structure(
    list(
      propensity = crossfit_propensity(task, learners_trt, control),
      population = crossfit_population(task, learners_pop, control),
      outcome = crossfit_outcome(task, learners_outcome, control)
    ),
    class = "standard")

  if (match.arg(estimator) == "standard") {
    psi <- influence_function(nuisance, task)
    return(output(psi, nuisance, call))
  }

  eif_ate <- influence_function(structure(nuisance, class = "ate"), task)

  nuisance$cate <- crossfit_cate(
    task$clone()$
      add_var(eif_ate@eif, "eif_ate"),
    learners_heterogeneity,
    control
  )

  nuisance$hodds <- crossfit_hodds(
    task$clone()$
      add_var(nuisance$cate$pred, "estimated_cate"),
    learners_heterogeneity,
    control
  )

  nuisance$heterogeneity <- crossfit_heterogeneity(
    task$clone()$
      add_var(nuisance$cate$pred, "estimated_cate")$
      add_var(nuisance$population$pred, "estimated_source_prob"),
    learners_heterogeneity,
    control
  )

  output(influence_function(structure(nuisance, class = "collaborative"), task),
         nuisance, call)
}
