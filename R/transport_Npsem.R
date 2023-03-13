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
        R = NULL,
        initialize = function(data, W, V = NULL, R = NULL, A, Z = NULL, S, Y) {
            checkmate::assertCharacter(W)
            checkmate::assertCharacter(V, null.ok = TRUE)
            checkmate::assertCharacter(Z, len = 1, null.ok = TRUE)
            checkmate::assertCharacter(A, len = 1)
            checkmate::assertCharacter(R, len = 1, null.ok = TRUE)
            checkmate::assertCharacter(S, len = 1)
            checkmate::assertCharacter(Y, len = 1)

            self$data <- data
            self$W <- unique(c(W, V, Z))
            self$R <- R
            self$V <- unique(c(V, Z))
            self$A <- A
            self$Z <- Z
            self$S <- S
            self$Y <- Y
        },
        #' Return a data frame or vector of the variable
        var = function(var = c("W", "V", "R", "A", "Z", "S", "Y"), data = FALSE, drop = FALSE) {
            if (!data) {
                return(self[[match.arg(var)]])
            }
            self$data[, self[[match.arg(var)]], drop = drop]
        },
        #' Get all parent nodes for a variable
        history = function(var = c("A", "R", "Y", "S"), data = FALSE) {
            vars <- switch(
                match.arg(var),
                A = private$parents_A(),
                S = private$parents_S(),
                R = private$parents_R(),
                Y = private$parents_Y()
            )

            if (!data) {
                return(vars)
            }
            self$data[, vars, drop = match.arg(var) == "Y"]
        },
        #' Return the names of all variables
        all_vars = function() {
            c(self$W, self$A, self$Z, self$V, self$R, self$S, self$Y)
        },
        modify = function(var, x) {
            mod <- self$clone()
            mod$data[[self[[var]]]] <- x
            mod
        }
    ),
    private = list(
        parents_S = function() {
            self$V
        },
        parents_A = function() {
            unique(c(self$S, self$W, self$R))
        },
        parents_R = function() {
            c(self$S, self$W)
        },
        parents_Y = function() {
            unique(c(self$A, self$W, self$R))
        }
    )
)
