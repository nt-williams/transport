#' @title Transport Task Class
#'
#' @description
#'
#' @export
#' @examples
TransportTask <- R6Class("TransportTask",
  public = list(
    outcome_type = NULL,
    data = NULL,
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

      self$data <- private$as_transport_data(data)
      self$outcome_type <- private$get_outcome_type()
      self$folds <- private$make_folds(folds)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat("Transport task\n")
      print(head(self$data))
    },

    #' @description
    #' Get training and validation sets for a fold.
    #' @param fold Fold number.
    training = function(fold) {
      TransportTaskSplit$new(
        self$data[self$folds[[fold]]$training_set, ],
        self$outcome_type,
        self$col_roles
      )
    },

    #' @description
    #' Get training and validation sets for a fold.
    #' @param fold Fold number.
    validation = function(fold) {
      TransportTaskSplit$new(
        self$data[self$folds[[fold]]$validation_set, ],
        self$outcome_type,
        self$col_roles
      )
    },

    #' @description
    #' Subset and return the task data, keeping only features from `cols`.
    #' @param cols A character vector of column names.
    select = function(cols) {
      assert_character(cols)
      assert_subset(cols, unlist(self$col_roles))
      self$data[, cols]
    },

    #' @description
    #' Subset and return the task data, keeping only features from `cols` that aren't censored.
    #' @param cols A character vector of column names.
    observed = function(cols) {
      obs <- self$select(self$col_roles$obs)
      if (is.null(obs)) {
        return(self$select(cols))
      }

      self$select(cols)[self$select(self$col_roles$obs) == 1, ]
    }
  ),
  private = list(
    as_transport_data = function(data) {
      assert_data_frame(data)
      assert_subset(unlist(self$col_roles), names(data))
      assert_binary(unique(self$select(self$col_roles$pop)))

      self$data <- data[, unlist(self$col_roles)]
    },
    make_folds = function(folds) {
      assert_number(folds, lower = 1, finite = TRUE)

      if (folds == 1) {
        folded <- list(list(
          v = 1,
          training_set = 1:nrow(self$data),
          validation_set = 1:nrow(self$data)
        ))

        return(folded)
      }

      if (is.null(self$col_roles$group) & self$outcome_type == "binomial") {
        strata <- self$select(self$col_roles$outcome)
        strata[is.na(strata)] <- 2
        folded <- origami::make_folds(self$data, V = folds, strata_ids = strata)
        return(folded)
      }

      if (!is.null(self$col_roles$group)) {
        folded <- origami::make_folds(self$data, cluster_ids = self$select(self$col_roles$group), V = folds)
        return(folded)
      }

      origami::make_folds(self$data, V = folds)
    },
    get_outcome_type = function() {
      target <- na.omit(self$select(self$col_roles$outcome))
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
      self$data <- data
      self$outcome_type <- outcome_type
      self$col_roles <- col_roles
      self$folds <- NULL
    }
  )
)
