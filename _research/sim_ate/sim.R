library(purrr)

gendata <- function(n, truth = FALSE) {
    S <- rbinom(n, 1, 0.5)

    if (truth) {
        S <- 0
    }

    W1 <- rbinom(n, 1, 0.4 + 0.2*S)
    W2 <- rbinom(n, 1, 0.1*S + 0.1)
    W3 <- rbinom(n, 1, 0.25 + 0.2*S)

    A <- rbinom(n, 1, 0.5)

    prob_Y1 <- plogis(log(0.4) + log(1.5) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3)
    gW <- plogis(log(0.4) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3)
    prob_Y <- A * prob_Y1 + (1 - A) * gW

    # fV <- prob_Y1 - gW
    # all(prob_Y == A * fV + gW)

    data.frame(W1 = W1,
               W2 = W2,
               W3 = W3,
               S = S,
               A = A,
               Y = rbinom(n, 1, prob_Y),
               Y0 = rbinom(n, 1, gW),
               Y1 = rbinom(n, 1, prob_Y1))
}

dat <- expand.grid(S = c(0, 1), W1 = c(0, 1), W2 = c(0, 1), W3 = c(0, 1))

dat$prob_W1 <- with(dat, W1 * (0.4 + 0.2*S) + (1 - W1) * (1 - (0.4 + 0.2*S)))
dat$prob_W2 <- with(dat, W2 * (0.1*S + 0.1) + (1 - W2) * (1 - (0.1*S + 0.1)))
dat$prob_W3 <- with(dat, W3 * (0.25 + 0.2*S) + (1 - W3) * (1 - (0.25 + 0.2*S)))
dat$prob_W <- with(dat, prob_W1 * prob_W2 * prob_W3)
dat$prob_SW <- with(dat, prob_W * 0.5)

dat$prob_Y1 <- with(dat, plogis(log(0.4) + log(1.5) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3))
dat$prob_Y0 <- with(dat, plogis(log(0.4) + log(1.7)*W1 - log(1.75)*W2 + log(1.8)*W3))

sum(dat$prob_SW * (dat$prob_Y1 - dat$prob_Y0))
truth <- lapply(split(dat, dat$S), function(x) sum(x$prob_W * (x$prob_Y1 - x$prob_Y0)))

n <- 1e4
K <- 250

res <- lapply(1:K, \(x) vector("numeric", 2))
for (k in 1:K) {
    dat <- gendata(n)

    Np <- transport_Npsem$new(dat, c("W1", "W2", "W3"),
                              Z = "A", S = "S", Y = "Y")

    ans <- try(transport_ate(Np,
                             c("SL.glm", "SL.xgboost", "SL.earth", "SL.glm.interaction"),
                             "binomial"))
    if (class(ans) == "try-error") next

    ci <- ans$theta + c(-1, 1) * qnorm(0.975) * sqrt(ans$var / n)

    pluck(res, k, 1) <- ans$theta
    pluck(res, k, 2) <- dplyr::between(truth[["0"]], ci[1], ci[2])
}

abs(map_dbl(res, 1) |> mean() - truth[["0"]]) * sqrt(n)
map_dbl(res, 2) |> mean()
