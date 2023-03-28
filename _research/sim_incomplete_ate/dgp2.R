gendata <- function(n, A = NULL) {
    W <- rbinom(n, 1, 0.5)
    V <- rbinom(n, 1, 0.66)
    Z <- rbinom(n, 1, 0.33)

    if (is.null(A)) A <- rbinom(n, 1, 0.5)

    S <- rbinom(n, 1, 0.5 - 0.4*W + 0.3*Z)

    Yi <- rnorm(n, A + W + A*V + 2.5*A*Z, sqrt((0.1 + 0.5*W)^2))
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W = W,
               V = V,
               Z = Z,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) - mean(subset(gendata(1e7, 0), S == 0)$Yi)
