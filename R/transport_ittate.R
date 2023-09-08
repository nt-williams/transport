#' Transport Estimator for the Intent-to-Treat Average Treatment Effect
#'
#' One-step transport estimator for the intent-to-treat average treatment effect (ITTATE)
#' under the general model that allows for all confounders to modify
#' the ATE.
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem.
#' @param instrument [\code{character(1)}]\cr
#'  The column name of the instrumental variable. This variable should be coded as 0 or 1.
#' @param trt [\code{character(1)}]\cr
#'  The column name of the treatment variable. This variable should be coded as 0 or 1.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param source [\code{character(1)}]\cr
#'  The column name of the population indicator. This variable should be coded as 0 or 1.
#' @param covar [\code{character}]\cr
#'  An vector containing the column names of baseline covariates to be
#'  included for adjustment.
#' @param cens [\code{character}]\cr
#'  An optional column name of a censoring indicator. If missingness in the outcome is
#'  present, must be provided. CURRENTLY IGNORED.
#' @param outcome_type [\code{character(1)}]\cr
#'  Outcome variable type (i.e., continuous, binomial).
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers. CURRENTLY IGNORED.
#' @param weights [\code{numeric(nrow(data))}]\cr
#'  An optional vector containing sampling weights. CURRENTLY IGNORED.
#' @param control [\code{list}]\cr See \code{.transport_ittate_control()}.
#'
#' @seealso [.transport_ittate_control()].
#'
#' @return
#' @export
#'
#' @examples
transport_ittate <- function(data, instrument, trt, outcome, source, covar, cens = NULL,
                             outcome_type = c("binomial", "continuous"),
                             id = NULL, weights = NULL,
                             control = .transport_ittate_control()) {
    checkmate::assert_character(instrument, len = 1)
    checkmate::assert_character(trt, len = 1)
    checkmate::assert_character(outcome, len = 1)
    checkmate::assert_character(source, len = 1)
    checkmate::assert_character(covar)
    checkmate::assert_character(cens, len = 1, null.ok = TRUE)
    checkmate::assert_character(id, len = 1, null.ok = TRUE)
    checkmate::assertSubset(c(instrument, trt, outcome, source, covar, id), names(data))

    folds <- make_folds(data, control$folds)

    # Fit the instrument-propensity score: P(A=1 | W,S=1)
    fit_IpiS1 <- crossfit(data = data,
                        target = instrument,
                        covar = covar,
                        folds = folds,
                        assignments = NULL,
                        outcome_type = "binomial",
                        learners = control$learners_trt,
                        cvfolds = control$folds_trt,
                        subset = data[[source]] == 1)

    # Fit the instrument-propensity score: P(A=1 | W,S=1)
    fit_IpiS0 <- crossfit(data = data,
                        target = instrument,
                        covar = covar,
                        folds = folds,
                        assignments = NULL,
                        outcome_type = "binomial",
                        learners = control$learners_trt,
                        cvfolds = control$folds_trt,
                        subset = data[[source]] == 0)

    # Fit the treatment-propensity score: P(Z=1 | A,S,W)
    fit_ApiS0 <- crossfit(data = data,
                       target = trt,
                       covar = c(instrument, covar),
                       folds = folds,
                       assignments = setNames(list(0, 1), rep(instrument, 2)),
                       outcome_type = "binomial",
                       learners = control$learners_trt,
                       cvfolds = control$folds_trt,
                       subset = data[[source]] == 0)

    # Fit the treatment-propensity score: P(Z=1 | A,S,W)
    fit_ApiS1 <- crossfit(data = data,
                        target = trt,
                        covar = c(instrument, covar),
                        folds = folds,
                        assignments = setNames(list(0, 1), rep(instrument, 2)),
                        outcome_type = "binomial",
                        learners = control$learners_trt,
                        cvfolds = control$folds_trt,
                        subset = data[[source]] == 1)

    # Fit the probability of being in the target population: P(S=1 | W)
    fit_S <- crossfit(data = data,
                      target = source,
                      covar = covar,
                      folds = folds,
                      assignments = NULL,
                      outcome_type = "binomial",
                      learners = control$learners_source,
                      cvfolds = control$folds_source)

    # Fit the outcome regression: E(Y|A,Z,W,S=1)
    fit_m <- crossfit(data = data,
                      target = outcome,
                      covar = c(instrument, trt, covar),
                      folds = folds,
                      assignments = setNames(list(NULL, 0, 1), rep(trt, 3)),
                      outcome_type = match.arg(outcome_type),
                      learners = control$learners_outcome,
                      cvfolds = control$folds_outcome,
                      subset = data[[source]] == 1)

    # Fit the pseudo regressions: E[E(Y | A,Z,W,S=1) | A,W,S=0]
    fit_pseudos <- lapply(1:3, function(j, subset, variables) {
        crossfit(data = cbind(data, foovar = fit_m$pred[, j, drop = FALSE]),
                 target = "foovar",
                 covar = variables[[j]],
                 folds = folds,
                 assignments = NULL,
                 outcome_type = "continuous",
                 learners = control$learners_outcome,
                 cvfolds = control$folds_outcome,
                 subset = data[[source]] == 0 & subset[[j]])
        },
        subset = list(rep(TRUE, nrow(data)),
                      tmp[[instrument]] == 0,
                      tmp[[instrument]] == 1),
        variables = list(c(covar, instrument), covar, covar)
    )

    eif <- eif_transport_ittate(data, instrument, trt, outcome, source, fit_IpiS0$pred,
                                fit_IpiS1$pred, fit_ApiS0$pred, fit_ApiS1$pred, fit_S$pred,
                                fit_m$pred, lapply(fit_pseudos, function(x) x$pred))

    list(estimates = eif,
         learner_weights_instrument_S0 = fit_IpiS0$weights,
         learner_weights_instrument_S1 = fit_IpiS1$weights,
         learner_weights_trt_S0 = fit_ApiS0$weights,
         learner_weights_trt_S1 = fit_ApiS1$weights,
         learner_weights_source = fit_S$weights,
         learner_weights_outcome = fit_m$weights)
}
