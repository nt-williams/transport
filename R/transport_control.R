#' Set Estimation Parameters
#'
#' Defines default parameters for estimators in the 'transport' package.
#'
#' @param .learners_folds \[\code{integer(1)}\]\cr
#'  The number of cross-validation folds for fitting nuisance parameters.
#' @param .bound \[\code{numeric(1)}\]\cr
#'  Determines that maximum and minimum values binomial predictions
#'  will be bounded by. The default is 1e-5, bounding predictions by 1e-5 and 0.9999.
#' @param .return_full_fits \[\code{logical(1)}\]\cr
#'  Return full 'mlr3superlearner' fits? Default is \code{FALSE}, return only 'mlr3superlearner' weights.
#' @param .discrete \[\code{logical(1)}\]\cr
#'  Use discrete or ensemble super learner? Default is \code{FALSE}.
#' @param .info \[\code{logical(1)}\]\cr
#'  Print super learner fitting info to the console? Default is \code{FALSE}.
#'
#' @return A list with parameter values.
#' @export
#'
#' @examples
#' transport_control(.learners_folds = 10)
transport_control <- function(.learners_folds = NULL,
                              .bound = 1e5,
                              .return_full_fits = FALSE,
                              .discrete = FALSE,
                              .info = FALSE) {
  assert_number(.learners_folds, null.ok = TRUE)
  assert_number(.bound)
  assert_logical(.return_full_fits, len = 1)
  assert_logical(.discrete)
  assert_logical(.info)

  list(.bound = .bound,
       .learners_folds = .learners_folds,
       .return_full_fits = .return_full_fits,
       .discrete = .discrete,
       .info = .info)
}
