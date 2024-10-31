#' Title
#'
#' @param .learners_folds
#' @param .bound
#' @param .return_full_fits
#' @param .discrete
#' @param .info
#'
#' @return
#' @export
#'
#' @examples
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
