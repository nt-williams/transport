TransportTask <- R6Class("TransportTask",
  public = list(
    backend = NULL,
    col_roles = NULL,
    folds = NULL,
    outcome_type = NULL,

    initialize = function(data, A, Y, W, S, C, id, weights, folds = 1) {
      self$col_roles <- TransportVars$new(W, S, A, C, Y, id, weights)
      self$backend <- private$as_transport_data(data)
      self$folds <- private$make_folds(folds)

      private$.row_copy <- 1:nrow(self$backend)
      private$.col_copy <- names(self$backend)
      private$.active_rows <- private$.row_copy
      private$.active_cols <- private$.col_copy
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
      i <- self$active_rows
      j <- self$active_cols

      if (reset) self$reset()

      if (is.null(j)) return(NULL)

      self$backend[i, j]
    },

    reset = function() {
      self$active_rows <- private$.row_copy
      self$active_cols <- private$.col_copy
      invisible(self)
    },

    select = function(cols) {
      assert_character(cols, null.ok = TRUE)
      self$active_cols <- intersect(self$active_cols, cols)
      invisible(self)
    },

    modify = function(col, x) {
      old <- self$backend[self$active_rows, col]
      on.exit(self$backend[self$active_rows, col] <- old)
      self$backend[self$active_rows, col] <- x
      self$data(reset = FALSE)
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
      assert_subset(self$col_roles$all(), names(data))
      assert_binary(data[[self$col_roles$A]])
      assert_binary(data[[self$col_roles$S]])
      assert_numeric(data[[self$col_roles$Y]], any.missing = TRUE)

      # need to add test for only missing allowed with S = 0 or C = 1

      self$outcome_type <- get_outcome_type(data, self$col_roles$Y)

      private$bounds <- y_bounds(data[[self$col_roles$Y]], self$outcome_type)
      data[[self$col_roles$Y]] <- self$scale(data[[self$col_roles$Y]])

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
    }
  )
)
