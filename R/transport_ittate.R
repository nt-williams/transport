#' Transported Intent-to-Treat Average Treatment Effect
#'
#' Implements a TMLE for the transported intent-to-treat average treatment effect.
#' Nuisance parameters are estimated using the Super Learner algorithm.
#'
#' @param data \[\code{data.frame}\]\cr
#'  A \code{data.frame} in containing all necessary variables for the estimation problem.
#' @param instrument \[\code{character(1)}\]
#'  The column name of the randomization variable. This variable must be binary (0/1).
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
#' @param learners_instrument \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the propensity score of the instrument.
#' @param learners_trt \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the propensity score of the treatment.
#' @param learners_pop \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the population mechanism.
#' @param learners_outcome \[\code{character}\]\cr
#'  A vector of \code{mlr3superlearner} algorithms for estimation of the outcome regression.
#' @param folds \[\code{integer(1)}\]\cr
#'  The number of folds to be used for cross-fitting.
#' @param control \[\code{list()}\]\cr
#'  Output of \code{transport_control()}.
#'
#' @returns An object of class \code{transported_ittate} containing the parameter estimate.
#' @export
#'
#' @example inst/examples/examples-transport_ittate.R
transport_ittate <- function(data, instrument, trt, outcome, covar, pop,
                             obs = NULL, id = NULL, weights = NULL,
                             learners_instrument = "glm",
                             learners_trt = "glm",
                             learners_pop = "glm",
                             learners_outcome = "glm",
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

  nuisance <- structure(
    list(
      propensity = crossfit(task, "propensity", learners_instrument, control),
      population = crossfit(task, "population", learners_pop, control),
      outcome    = crossfit(task, "outcome", learners_outcome, control),
      ptc        = crossfit(task, "ptc", learners_trt, control)
    ),
    class = "ittate"
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

  output(influence_function(nuisance, task),
         nuisance,
         call,
         "transported_ittate")
}
