TransportVars <- R6Class("TransportVars",
  public = list(
    W = NULL,
    S = NULL,
    A = NULL,
    Z = NULL,
    C = NULL,
    Y = NULL,
    id = NULL,
    weights = NULL,
    initialize = function(W, S, A, Z, C, Y, id, weights) {
      assert_character(A, len = 1)
      assert_character(Z, len = 1, null.ok = TRUE)
      assert_character(W, null.ok = TRUE)
      assert_character(C, len = 1, null.ok = TRUE)
      assert_character(Y, len = 1)
      assert_character(S, len = 1)
      assert_character(id, len = 1, null.ok = TRUE)
      assert_character(weights, len = 1, null.ok = TRUE)

      self$W <- W
      self$S <- S
      self$A <- A
      self$Z <- Z
      self$C <- C
      self$Y <- Y
      self$id <- id
      self$weights <- weights
    },

    history = function(var = c("S", "A", "Y", "Z")) {
      switch(match.arg(var),
        S = private$parents_S(),
        A = private$parents_A(),
        Z = private$parents_Z(),
        Y = private$parents_Y()
      )
    },

    all = function() {
      c(self$S, self$W, self$A, self$Z, self$C, self$Y, self$id, self$weights)
    }
  ),
  private = list(
    parents_S = function() {
      self$W
    },
    parents_A = function() {
      c(self$S, self$W)
    },
    parents_Z = function() {
      c(self$S, self$W, self$A)
    },
    parents_Y = function() {
      c(self$A, self$W, self$Z)
    }
  )
)
