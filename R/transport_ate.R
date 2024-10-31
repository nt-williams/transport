#' Transported Average Treatment Effect
#'
#' Implements two one-step estimators for the transported average treatment effect for a binary or continuous outcome.
#' Nuisance parameters are estimated using the Super Learner algorithm.
#'
#' @param data \[\code{data.frame}\]\cr
#'  A \code{data.frame} in containing all necessary variables for the estimation problem.
#' @param trt \[\code{character(1)}\]
#'  The column name of the treatment variable. This variable must be binary (0/1).
#' @param outcome \[\code{character(1)}\]\cr
#'  The column name of the outcome variable. This variable must be binary (0/1) or numeric.
#' @param covar \[\code{character}\]\cr
#'  An optional vector containing the column names of covariates to be
#'  included for adjustment.
#' @param pop \[\code{character(1)}\]\cr
#'  The column name of the population indicator variable.
#'  This variable must be binary (0/1) with 0 indicating the target population and 1 the source population.
#' @param obs \[\code{character(1)}\]\cr
#'  An optional column name for a censoring indicator the same. If missingness in the outcome is
#'  present, must be provided. This variable must be binary (0/1) with 1 indicating that the outcome is observed.
#' @param id  \[\code{character(1)}\]\cr
#'  An optional column name containing cluster level identifiers.
#' @param weights \[\code{character(1)}\]\cr
#'  An optional column name containing sampling weights. Currently not used.
#' @param estimator \[\code{character(1)}\]\cr
#'  The estimator to use. See details for more information.
#' @param learners_trt \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the propensity score.
#' @param learners_pop \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the population mechanism.
#' @param learners_outcome \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the outcome regression.
#' @param learners_heterogeneity \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of collaborative nuisance parameters.
#'  Ignored when \code{estimator = "standard"}.
#' @param folds \[\code{integer(1)}\]\cr
#'  The number of folds to be used for cross-fitting.
#' @param control \[\code{list()}\]\cr
#'  Output of \code{transport_control()}.
#'
#' @details
#' ## Estimators
#' *TODO*
#'
#' @return An object of class \code{transported_ate} containing the parameter estimate.
#'
#' @export
#'
#' @example inst/examples/examples-transport_ate.R
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
      propensity = crossfit(task, "propensity", learners_trt, control),
      population = crossfit(task, "population", learners_pop, control),
      outcome    = crossfit(task, "outcome", learners_outcome, control)
    ),
    class = "standard")

  if (match.arg(estimator) == "standard") {
    psi <- influence_function(nuisance, task)
    return(output(psi, nuisance, call))
  }

  eif_ate <- influence_function(structure(nuisance, class = "ate"), task)

  nuisance$cate <- crossfit(
    task$clone()$
      add_var(eif_ate@eif, "eif_ate"),
    "cate",
    learners_heterogeneity,
    control
  )

  nuisance$hodds <- crossfit(
    task$clone()$
      add_var(nuisance$cate$pred, "estimated_cate"),
    "hodds",
    learners_heterogeneity,
    control
  )

  nuisance$heterogeneity <- crossfit(
    task$clone()$
      add_var(nuisance$cate$pred, "estimated_cate")$
      add_var(nuisance$population$pred, "estimated_source_prob"),
    "heterogeneity",
    learners_heterogeneity,
    control
  )

  output(influence_function(structure(nuisance, class = "collaborative"), task),
         nuisance, call)
}
