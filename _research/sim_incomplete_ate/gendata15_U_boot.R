suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyverse))
library(furrr)
library(transport)
library(boot)

gendata <- function(n, A = NULL) {
    W1 <- rbinom(n, 1, 0.5)
    W2 <- rbinom(n, 1, 0.25)

    if (is.null(A)) {
        A <- rbinom(n, 1, 0.5)
    }

    S <- rbinom(n, 1, 0.8 - 0.5*W1 - 0.1*W2)

    Yi <- rnorm(n, 1.2 + 0.25*A + 0.5*W1 + A*W1 + 0.5*W2)
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W1 = W1,
               W2 = W2,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) -
    mean(subset(gendata(1e7, 0), S == 0)$Yi)

covered <- function(low, high) dplyr::between(truth, low, high)

boot1 <- function(data, i) {
    Np <- transport_Npsem$new(data[i, ], c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    results <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                               "gaussian", "adaptive-lasso")
    c(results$theta, results$ipw)
}

boot2 <- function(data, i) {
    Np <- transport_Npsem$new(data[i, ], c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    results <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                               "gaussian", "adaptive-lasso-sl")
    c(results$theta, results$ipw)
}

boot3 <- function(data, i) {
    Np <- transport_Npsem$new(data[i, ], c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    results <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.xgboost"),
                                               "gaussian", "sl")
    c(results$theta, results$ipw)
}

safe_sim <- possibly(function(n) {
    dat <- gendata(n)

    bts1 <- boot::boot(dat, boot1, 100)
    ci1 <- boot::boot.ci(bts1, type = "norm", index = 1)
    ci1_ipw <- boot::boot.ci(bts1, type = "norm", index = 2)

    bts2 <- boot::boot(dat, boot2, 100)
    ci2 <- boot::boot.ci(bts2, type = "norm", index = 1)
    ci2_ipw <- boot::boot.ci(bts2, type = "norm", index = 2)

    bts3 <- boot::boot(dat, boot3, 100)
    ci3 <- boot::boot.ci(bts3, type = "norm", index = 1)
    ci3_ipw <- boot::boot.ci(bts3, type = "norm", index = 2)

    data.frame(estimator = c("gamma1", "gamma1_ipw", "gamma2", "gamma2_ipw", "gamma3", "gamma3_ipw"),
               psi = c(bts1$t0[1], bts1$t0[2], bts2$t0[1], bts2$t0[2], bts3$t0[1], bts3$t0[2]),
               var = c((sd(bts1$t[, 1])^2)*n, (sd(bts1$t[, 2])^2)*n,
                       (sd(bts2$t[, 1])^2)*n, (sd(bts2$t[, 2])^2)*n,
                       (sd(bts3$t[, 1])^2)*n, (sd(bts3$t[, 2])^2)*n),
               covered = c(covered(ci1$normal[1, 2], ci1$normal[1, 3]),
                           covered(ci1_ipw$normal[1, 2], ci1_ipw$normal[1, 3]),
                           covered(ci2$normal[1, 2], ci2$normal[1, 3]),
                           covered(ci2_ipw$normal[1, 2], ci2_ipw$normal[1, 3]),
                           covered(ci3$normal[1, 2], ci3$normal[1, 3]),
                           covered(ci3_ipw$normal[1, 2], ci3_ipw$normal[1, 3])))
}, NULL)

plan(multisession)

res <- map_dfr(c(`100` = 100, `1000` = 1000, `1e4` = 1e4), function(n) {
        future_map_dfr(1:100, function(i) safe_sim(n), .id = "i")
    }, .id = "n") |>
    mutate(n = as.numeric(n))

plan(sequential)

out <- group_by(res, n, estimator) |>
    summarise(absbias = abs(mean(psi) - truth),
              nvar = var(psi),
              coverage = mean(covered),
              estimvar = mean(var)) |>
    mutate(nvar = nvar * n) |>
    ungroup()

saveRDS(out, "_research/sim_incomplete_ate/results/dgp5_boot.rds")
