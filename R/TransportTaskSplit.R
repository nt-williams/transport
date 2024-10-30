TransportTaskSplit <- R6Class("TransportTaskSplit",
  inherit = TransportTask,
  public = list(
    type = NULL,
    initialize = function(x, type) {
      self$type <- type
      self$backend <- x$data(reset = FALSE)
      self$outcome_type <- x$outcome_type
      self$col_roles <- x$col_roles
      private$.row_copy <- 1:nrow(self$backend)
      private$.col_copy <- names(self$backend)
      self$active_rows <- private$.row_copy
      self$active_cols <- private$.col_copy
    }
  )
)
