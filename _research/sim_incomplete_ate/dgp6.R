gendata <- function(n, A = NULL) {
    W <- rbinom(n, 1, 0.5)
    Z <- rbinom(n, 1, 0.25)
    V1 <- rbinom(n, 1, 0.75)
    V2 <- rbinom(n, 1, 0.33)

    if (is.null(A)) {
        A <- rbinom(n, 1, 0.5)
    }

    S <- rbinom(n, 1, 0.8 - 0.5*Z - 0.25*W)

    Yi <- rnorm(n, 1.2 + 0.25*A + 0.5*W + A*Z + 0.5*Z + 0.4*A*V1 - 0.75*A*V2)
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W = W,
               Z = Z,
               V1 = V1,
               V2 = V2,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) -
    mean(subset(gendata(1e7, 0), S == 0)$Yi)
