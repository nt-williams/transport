sim_data <- function(n) {
    S <- rbinom(n, 1, 0.5)
    W1 <- rbinom(n, 1, 0.4 + (0.2 * S))
    W2 <- rnorm(n, 0.1*S, 1)
    W3 <- rnorm(n, 1 + (0.2*S), 1)
    A <- rbinom(n, 1, plogis(-log(1.6) +log(4) - log(1.1)*W2  -log(1.3)*W3))
    Y0 <- rbinom(n, 1, plogis(log(1.6) - log(1.3)*W3 - log(1.2)*W1))
    Y1 <- rbinom(n, 1, plogis(log(1.6) + log(1.9) -log(1.3)*W3 - log(1.2)*W1 + log(1.2)*W1))
    Y <- ifelse(A == 1, Y1, Y0)
    list(obs = data.frame(W1 = W1, W2 = W2, W3 = W3, S = S, A = A, Y = Y, Y1 = Y1, Y0 = Y0),
         ate = mean((Y1 - Y0)),
         ate_S0 = mean((Y1 - Y0)[S == 0]),
         ate_S1 = mean((Y1 - Y0)[S == 1]))
}

truth <- mean(vapply(1:1000, \(x) sim_data(1e4)$ate_S0, FUN.VALUE = 1))

devtools::load_all()

K <- 1000
n <- 1000
#res <- vector("numeric", K)
res <- lapply(1:K, \(x) vector("numeric", 2))
for (k in 1:K) {
    dat <- sim_data(n)
    obs <- dat$obs
    Np <- transport_Npsem$new(dat$obs,
        c("W1", "W2", "W3"),
        V = c("W1", "W3"), A = NULL,
        Z = "A", S = "S", Y = "Y"
    )
    ans <- transport_ate_incomplete(Np, c("SL.glm.interaction"), "binomial")
    res[[k]][1] <- mean(ans$eif_1 - ans$eif_0)
    res[[k]][2] <- var(ans$eif_1 - ans$eif_0) / n
}

hist(vapply(res, \(x) x[1], FUN.VALUE = 1), breaks = 20)
mean(vapply(res, \(x) x[1], FUN.VALUE = 1))
var(vapply(res, \(x) x[1], FUN.VALUE = 1))

lapply(res, \(x) x[1] + (c(-1, 1) * qnorm(0.975)*sqrt(0.002480822))) |>
    vapply(\(x) dplyr::between(truth, x[1], x[2]), FUN.VALUE = 1) |>
    mean()
