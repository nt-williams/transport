transport_control <- function(.learners_outcome_folds = NULL,
                              .learners_trt_folds = NULL,
                              .learners_pop_folds = NULL,
                              .bound = 1e5,
                              .return_full_fits = FALSE,
                              .discrete = FALSE,
                              .info = FALSE) {
  assert_number(.learners_outcome_folds, null.ok = TRUE)
  assert_number(.learners_trt_folds, null.ok = TRUE)
  assert_number(.learners_pop_folds, null.ok = TRUE)
  assert_number(.bound)
  assert_logical(.return_full_fits, len = 1)
  assert_logical(.discrete)
  assert_logical(.info)

  list(.bound = .bound,
       .learners_outcome_folds = .learners_outcome_folds,
       .learners_trt_folds = .learners_trt_folds,
       .return_full_fits = .return_full_fits,
       .discrete = .discrete,
       .info = .info)
}
