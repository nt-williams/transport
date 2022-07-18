transport_Npsem <- R6::R6Class(
    "transport_Npsem",
    public = list(
        data = NULL,
        W = NULL,
        V = NULL,
        A = NULL,
        Z = NULL,
        S = NULL,
        Y = NULL,
        initialize = function(data, W, V = NULL, A, Z, S, Y) {
            checkmate::assertCharacter(W)
            checkmate::assertCharacter(V, null.ok = TRUE)
            checkmate::assertCharacter(Z, len = 1, null.ok = TRUE)
            checkmate::assertCharacter(A, len = 1)
            checkmate::assertCharacter(S, len = 1)
            checkmate::assertCharacter(Y, len = 1)

            self$data <- data
            self$W <- W
            self$V <- V
            self$A <- A
            self$Z <- Z
            self$S <- S
            self$Y <- Y
        },
        #' Return a data frame or vector of the variable
        var = function(var = c("W", "V", "A", "Z", "S", "Y")) {
            self$data[, self[[match.arg(var)]]]
        },
        #' Get all parent nodes for a variable
        history = function(var = c("A", "Z", "Y", "S")) {
            vars <- switch(
                match.arg(var),
                A = private$parents_A(),
                S = private$parents_S(),
                Z = private$parents_Z(),
                Y = private$parents_Y()
            )

            self$data[, vars, drop = match.arg(var) == "Y"]
        },
        #' Return the names of all variables
        all_vars = function() {
            c(self$W, self$A, self$Z, self$S, self$Y)
        },
        modify = function(var, x) {
            mod <- self$data
            mod[[self[[var]]]] <- x
            mod
        }
    ),
    private = list(
        parents_S = function() {
            self$V
        },
        parents_A = function() {
            c(self$S, self$W)
        },
        parents_Z = function() {
            c(self$W, self$A)
        },
        parents_Y = function() {
            c(private$parents_Z(), self$Z)
        }
    )
)
