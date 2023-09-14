#' Transport Estimator for the Average Treatment Effect using Known Effect Modifiers
#'
#' TODO
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem.
#' @param trt [\code{character(1)}]\cr
#'  The column name of the treatment variable. This variable should be coded as 0 or 1.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param source [\code{character(1)}]\cr
#'  The column name of the population indicator. This variable should be coded as 0 or 1.
#' @param covar [\code{character}]\cr
#'  An vector containing the column names of baseline covariates to be
#'  included for adjustment.
#' @param effect_modifiers [\code{character}]\cr
#' @param cens [\code{character}]\cr
#'  An optional column name of a censoring indicator. If missingness in the outcome is
#'  present, must be provided. CURRENTLY IGNORED.
#' @param outcome_type [\code{character(1)}]\cr
#'  Outcome variable type (i.e., continuous, binomial).
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers. CURRENTLY IGNORED.
#' @param weights [\code{numeric(nrow(data))}]\cr
#'  An optional vector containing sampling weights. CURRENTLY IGNORED.
#' @param control [\code{list}]\cr See \code{.transport_ate_control()}.
#'
#' @seealso [.transport_ate2_control()].
#'
#' @return
#' @export
#'
#' @examples
transport_ate2 <- function(data, trt, outcome, source, covar,
                           effect_modifiers, cens = NULL,
                           outcome_type = c("binomial", "continuous"),
                           id = NULL, weights = NULL,
                           control = .transport_ate2_control()) {
    checkmate::assert_character(trt, len = 1)
    checkmate::assert_character(outcome, len = 1)
    checkmate::assert_character(source, len = 1)
    checkmate::assert_character(covar)
    checkmate::assert_character(effect_modifiers)
    checkmate::assert_character(cens, len = 1, null.ok = TRUE)
    checkmate::assert_character(id, len = 1, null.ok = TRUE)
    checkmate::assertSubset(effect_modifiers, covar)
    checkmate::assertSubset(c(trt, outcome, source, covar, id, effect_modifiers), names(data))

    folds <- make_folds(data, control$folds)

    # Fit the propensity score: P(A = 1|S = 1, W)
    fit_pi <- crossfit(data = data,
                       target = trt,
                       covar = covar,
                       folds = folds,
                       assignments = NULL,
                       outcome_type = "binomial",
                       learners = control$learners_trt,
                       cvfolds = control$folds_trt,
                       subset = data[[source]] == 1)

    # Fit the probability of being in the target population: P(S=1 | V)
    fit_S <- crossfit(data = data,
                      target = source,
                      covar = effect_modifiers,
                      folds = folds,
                      assignments = NULL,
                      outcome_type = "binomial",
                      learners = control$learners_source,
                      cvfolds = control$folds_source)

    # Fit the outcome regression: E(Y| S=1, V, W)
    fit_m <- crossfit(data = data,
                      target = outcome,
                      covar = c(trt, covar),
                      folds = folds,
                      assignments = setNames(list(0, 1), rep(trt, 2)),
                      outcome_type = match.arg(outcome_type),
                      learners = control$learners_outcome,
                      cvfolds = control$folds_outcome,
                      subset = data[[source]] == 1)

    T_OP <- eif_TOP(data, trt, source, outcome, fit_pi$pred, fit_m$pred)

    # Fit the pseudo regression: E(T(O;P) | V,S=1)
    fit_fV <- crossfit(data = cbind(data, foovar = T_OP),
                       target = "foovar",
                       covar = effect_modifiers,
                       folds = folds,
                       assignments = NULL,
                       outcome_type = "continuous",
                       learners = control$learners_pseudo,
                       cvfolds = control$folds_pseudo,
                       subset = data[[source]] == 1)

    eif <- eif_transport_ate2(data, trt, outcome, source,
                              fit_pi$pred, fit_S$pred, fit_m$pred, fit_fV$pred)

    list(estimates = eif,
         learner_weights_outcome = fit_m$weights,
         learner_weights_trt = fit_pi$weights,
         learner_weights_source = fit_S$weights,
         learner_weights_pseudo = fit_fV$weights)
}
