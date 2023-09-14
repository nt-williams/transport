#' Transport Estimator for the Complier Average Treatment Effect
#'
#' One-step transport estimator for the complier average treatment effect (CATE)
#' under the general model that allows for all confounders to modify
#' the ATE.
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
#' @param cens [\code{character}]\cr
#'  An optional column name of a censoring indicator. If missingness in the outcome is
#'  present, must be provided. CURRENTLY IGNORED.
#' @param outcome_type [\code{character(1)}]\cr
#'  Outcome variable type (i.e., continuous, binomial).
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers. CURRENTLY IGNORED.
#' @param weights [\code{numeric(nrow(data))}]\cr
#'  An optional vector containing sampling weights. CURRENTLY IGNORED.
#' @param control [\code{list}]\cr See \code{.transport_cate_control()}.
#'
#' @seealso [.transport_cate_control()].
#'
#' @return
#' @export
#'
#' @examples
transport_cate <- function(data, instrument, trt, outcome, source, covar, cens = NULL,
                          outcome_type = c("binomial", "continuous"),
                          id = NULL, weights = NULL,
                          control = .transport_cate_control()) {
    ittate <- transport_ittate(data = data,
                               instrument = instrument,
                               trt = trt,
                               outcome = outcome,
                               source = source,
                               covar = covar,
                               cens = cens,
                               outcome_type = match.arg(outcome_type),
                               id = NULL,
                               weights = NULL,
                               control = .transport_cate_control())

    eif <- eif_transport_cace(data, instrument, trt, source,
                              pi_I0 = ittate$pred_instrument_S0,
                              pi_trt0 = ittate$pred_trt_S0,
                              eif_ittate = ittate$estimates)

    out <- list()

    class(out) <- c("transport", "transport_cace")
    out
}

print.transport_cace <- function() {

}
