TransportTask <- R6Class("TransportTask",
  public = list(
    backend = NULL,
    col_roles = NULL,
    folds = NULL,
    outcome_type = NULL,

    initialize = function(data, A, Y, W, S, C, id, weights, folds = 1) {
      self$col_roles <- TransportVars$new(W, S, A, C, Y, id, weights)

      self$backend <- private$as_transport_data(data)
      self$outcome_type <- private$get_outcome_type()
      self$folds <- private$make_folds(folds)

      private$.row_copy <- 1:nrow(self$backend)
      private$.col_copy <- names(self$backend)
      private$.row_roles <- private$.row_copy
      private$.col_roles <- private$.col_copy
    },

    history = function(var) {
      self$col_roles$history(var)
    },

    features = function(model) {
      c(self$col_roles$id, self$history(model))
    },

    training = function(fold) {
      self$active_rows <- self$folds[[fold]]$training_set
      TransportTaskSplit$new(self, "train")
    },

    validation = function(fold) {
      self$active_rows <- self$folds[[fold]]$validation_set
      TransportTaskSplit$new(self, "valid")
    },

    data = function(reset = TRUE) {
      i <- private$.active_rows
      j <- private$.active_cols

      if (reset) self$reset()

      self$backend[i, j]
    },

    reset = function() {
      private$.active_rows <- private$.row_copy
      private$.active_cols <- private$.col_copy
      invisible(self)
    },

    select = function(cols) {
      assert_character(cols)
      private$.active_cols <- intersect(private$.active_cols, cols)
      invisible(self)
    },

    modify = function(col, x) {
      self$backend[self$active_rows, col] <- x
      invisible(self)
    },

    obs = function() {
      if (is.null(self$col_roles$C)) {
        return(invisible(self))
      }

      obs <- self$backend[[self$col_roles$C]]
      self$active_rows <- intersect(self$active_rows, which(obs == 1))
      invisible(self)
    },

    pop = function(x = c("source", "target")) {
      pop <- self$backend[[self$col_roles$S]]
      if (match.arg(x) == "source") {
        self$active_rows <- intersect(self$active_rows, which(pop == 0))
      } else {
        self$active_rows <- intersect(self$active_rows, which(pop == 1))
      }
      invisible(self)
    },

    nrow = function() {
      nrow(self$backend[self$active_rows, ])
    },

    nfolds = function() {
      length(self$folds)
    },

    scale = function(x) {
      (x - private$bounds[1]) / (private$bounds[2] - private$bounds[1])
    },

    rescale = function(x) {
      (x*(private$bounds[2] - private$bounds[1])) + private$bounds[1]
    }
  ),
  active = list(
    active_rows = function(rhs) {
      if (missing(rhs)) {
        return(private$.active_rows)
      }
      private$.active_rows <- rhs
    },

    active_cols = function(rhs) {
      if (missing(rhs)) {
        return(private$.active_cols)
      }
      private$.active_cols <- rhs
    }
  ),
  private = list(
    .row_copy = NULL,
    .col_copy = NULL,
    .active_rows = NULL,
    .active_cols = NULL,
    bounds = NULL,
    as_transport_data = function(data) {
      assert_data_frame(data)
      assert_subset(unlist(self$col_roles), names(data))
      assert_binary(unique(data[[self$col_roles$trt]]))
      data[, self$col_roles$all()]
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

      if (is.null(self$col_roles$id) & self$outcome_type == "binomial") {
        strata <- self$backend[[self$col_roles$Y]]
        strata[is.na(strata)] <- 2
        folded <- origami::make_folds(self$backend, V = folds, strata_ids = strata)
        return(folded)
      }

      if (!is.null(self$col_roles$id)) {
        folded <- origami::make_folds(self$backend, cluster_ids = self$backend[[self$col_roles$id]], V = folds)
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
