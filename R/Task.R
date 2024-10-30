#' @title Transport Task Class
#'
#' @description
#'
#' @export
#' @examples
TransportTask <- R6Class("TransportTask",
  public = list(
    outcome_type = NULL,
    backend = NULL,
    col_roles = NULL,
    folds = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(data, trt, outcome, covar, pop,
                          modifiers = NULL, obs = NULL,
                          group = NULL, weights = NULL, folds = 1) {
      self$col_roles <- list(
        trt = trt,
        outcome = outcome,
        covar = covar,
        pop = pop,
        modifiers = modifiers,
        obs = obs,
        group = group,
        weights = weights
      )

      self$backend <- private$as_transport_data(data)
      self$outcome_type <- private$get_outcome_type()
      self$folds <- private$make_folds(folds)

      private$.row_copy <- 1:nrow(self$backend)
      private$.col_copy <- names(self$backend)
      private$.row_roles <- private$.row_copy
      private$.col_roles <- private$.col_copy
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat("Transport task\n")
      print(head(self$backend))
    },

    #' @description
    #' Get training and validation sets for a fold.
    #' @param fold Fold number.
    training = function(fold) {
      TransportTaskSplit$new(
        self$backend[self$folds[[fold]]$training_set, ],
        self$outcome_type,
        self$col_roles
      )
    },

    #' @description
    #' Get training and validation sets for a fold.
    #' @param fold Fold number.
    validation = function(fold) {
      TransportTaskSplit$new(
        self$backend[self$folds[[fold]]$validation_set, ],
        self$outcome_type,
        self$col_roles
      )
    },

    data = function() {
      i <- private$.row_roles
      j <- private$.col_roles
      private$.row_roles <- private$.row_copy
      private$.col_roles <- private$.col_copy
      self$backend[i, j]
    },

    reset = function() {
      private$.row_roles <- private$.row_copy
      private$.col_roles <- private$.col_copy
      invisible(self)
    },

    #' @description
    #' Subset and return the task data, keeping only features from `cols`.
    #' @param cols A character vector of column names.
    select = function(cols) {
      assert_character(cols)
      assert_subset(cols, unlist(self$col_roles))
      private$.col_roles <- intersect(private$.col_roles, cols)
      invisible(self)
    },

    #' @description
    #' Subset and return the task data, keeping only rows that aren't censored.
    obs = function() {
      if (is.null(self$col_roles$obs)) {
        return(invisible(self))
      }

      obs <- self$backend[[self$col_roles$obs]][private$.row_roles]
      private$.row_roles <- intersect(private$.row_roles, which(obs))
      invisible(self)
    },

    pop = function(x = c("source", "target")) {
      pop <- self$backend[[self$col_roles$pop]][private$.row_roles]
      if (match.arg(x) == "source") {
        private$.row_roles <- intersect(private$.row_roles, which(pop == 0))
      } else {
        private$.row_roles <- intersect(private$.row_roles, which(pop == 1))
      }
      invisible(self)
    }
  ),
  private = list(
    .row_copy = NULL,
    .col_copy = NULL,
    .row_roles = NULL,
    .col_roles = NULL,
    as_transport_data = function(data) {
      assert_data_frame(data)
      assert_subset(unlist(self$col_roles), names(data))
      assert_binary(unique(data[[self$col_roles$trt]]))
      assert_binary(unique(data[[self$col_roles$trt]]))
      data[, unlist(self$col_roles)]
    },
    make_folds = function(folds) {
      assert_number(folds, lower = 1, finite = TRUE)

      if (folds == 1) {
        folded <- list(list(
          v = 1,
          training_set = 1:nrow(self$backend),
          validation_set = 1:nrow(self$backend)
        ))

        return(folded)
      }

      if (is.null(self$col_roles$group) & self$outcome_type == "binomial") {
        strata <- self$backend[[self$col_roles$outcome]]
        strata[is.na(strata)] <- 2
        folded <- origami::make_folds(self$backend, V = folds, strata_ids = strata)
        return(folded)
      }

      if (!is.null(self$col_roles$group)) {
        folded <- origami::make_folds(self$backend, cluster_ids = self$backend[[self$col_roles$group]], V = folds)
        return(folded)
      }

      origami::make_folds(self$backend, V = folds)
    },
    get_outcome_type = function() {
      target <- na.omit(self$backend[[self$col_roles$outcome]])
      unique_values <- unique(target)

      assert_binary(unique_values)

      if (!all(target %in% c(0, 1))) {
        return("continuous")
      }

      "binary"
    }
  )
)

TransportTaskSplit <- R6Class("TransportTaskSplit",
  inherit = TransportTask,
  public = list(
    initialize = function(data, outcome_type, col_roles) {
      self$backend <- data
      self$outcome_type <- outcome_type
      self$col_roles <- col_roles
      self$folds <- NULL

      private$.row_copy <- 1:nrow(self$backend)
      private$.col_copy <- names(self$backend)
      private$.row_roles <- private$.row_copy
      private$.col_roles <- private$.col_copy
    }
  )
)
