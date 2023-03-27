gendata <- function(n, A = NULL) {
    W <- rbinom(n, 1, 0.5)
    Z <- rbinom(n, 1, 0.25)
    V <- rbinom(n, 1, 0.75)

    if (is.null(A)) {
        A <- rbinom(n, 1, 0.5)
    }

    S <- rbinom(n, 1, 0.8 - 0.5*Z - 0.1*W)

    Yi <- rnorm(n, 1.2 + 0.25*A + 0.5*W + A*Z + 0.5*Z + 0.4*A*V)
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W = W,
               Z = Z,
               V = V,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) -
    mean(subset(gendata(1e7, 0), S == 0)$Yi)

dat <- gendata(1000)

folds <- 1

Np <- transport_Npsem$new(dat, c("W"), V = c("V"), Z = c("Z"), A = "A", S = "S", Y = "Y")
transport_ate_incomplete1(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)

Np <- transport_Npsem$new(dat, c("W"), V = c("V", "Z"), A = "A", S = "S", Y = "Y")
transport_ate_incomplete(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)

Np <- transport_Npsem$new(dat, c("W", "V", "Z"), A = "A", S = "S", Y = "Y")
transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.lightgbm"), "gaussian", "sl", folds)

Np <- transport_Npsem$new(dat, c("W", "Z", "V"), A = "A", S = "S", Y = "Y")
transport_ate(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.lightgbm"), "gaussian", folds)
