library(purrr)

gendata_effect_modification <- function(n) {
    S <- rbinom(n, 1, 0.5)
    W1 <- rbinom(n, 1, 0.4 + 0.2*S)
    W2 <- rbinom(n, 1, 0.1*S + 0.1)
    W3 <- rbinom(n, 1, 0.25 + 0.2*S)

    A <- rbinom(n, 1, 0.5)

    gW <- plogis(log(0.4) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3)
    fV <- plogis(log(0.2) + log(1.66)*W1)
    prob_Y <- A * fV + gW

    Y <- ifelse(S == 1, rbinom(n, 1, prob_Y), NA_real_)
    # theta_init <- mean(fV[S == 0])

    data.frame(W1 = W1,
               W2 = W2,
               W3 = W3,
               S = S,
               A = A,
               Y = Y,
               fV = fV)
}

dat <- expand.grid(S = c(0, 1), W1 = c(0, 1), W2 = c(0, 1), W3 = c(0, 1))

dat$prob_W1 <- with(dat, W1 * (0.4 + 0.2*S) + (1 - W1) * (1 - (0.4 + 0.2*S)))
dat$prob_W2 <- with(dat, W2 * (0.1*S + 0.1) + (1 - W2) * (1 - (0.1*S + 0.1)))
dat$prob_W3 <- with(dat, W3 * (0.25 + 0.2*S) + (1 - W3) * (1 - (0.25 + 0.2*S)))
dat$prob_W <- with(dat, prob_W1 * prob_W2 * prob_W3)

dat$gW <- with(dat, plogis(log(0.4) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3))
dat$fV <- with(dat, plogis(log(0.2) + log(1.66)*W1))

truth <- lapply(split(dat, dat$S), function(x) sum(x$prob_W * x$fV))

n <- 5000
K <- 500

res <- lapply(1:K, \(x) vector("numeric", 2))
for (k in 1:K) {
    dat <- gendata_effect_modification(n)

    Np <- transport_Npsem$new(dat, c("W1", "W2", "W3"),
                              V = c("W1"), A = NULL,
                              Z = "A", S = "S", Y = "Y")

    ans <- try(transport_ate_incomplete3(Np,
                                         c("SL.glm", "SL.xgboost", "SL.earth", "SL.glm.interaction"),
                                         "binomial"))
    if (class(ans) == "try-error") next

    ci <- ans$theta + c(-1, 1) * qnorm(0.975) * sqrt(ans$var / n)

    pluck(res, k, 1) <- ans$theta
    pluck(res, k, 2) <- dplyr::between(truth[["0"]], ci[1], ci[2])
}

abs(map_dbl(res, 1) |> mean() - truth[["0"]]) * sqrt(n)
map_dbl(res, 2) |> mean()
