gendata <- function(n, A = NULL) {
    W1 <- rbinom(n, 1, 0.5)

    if (is.null(A)) A <- rbinom(n, 1, 0.5)

    S <- rbinom(n, 1, 0.8 - 0.7*W1)

    Yi <- rnorm(n, A + W1, sqrt((0.1 + 0.8*W1)^2))
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W1 = W1,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) - mean(subset(gendata(1e7, 0), S == 0)$Yi)
