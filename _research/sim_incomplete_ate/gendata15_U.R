suppressPackageStartupMessages(library(tidyverse))
library(furrr)
library(transport)

gendata <- function(n, A = NULL) {
    W1 <- rbinom(n, 1, 0.5)
    W2 <- rbinom(n, 1, 0.25)

    if (is.null(A)) {
        A <- rbinom(n, 1, 0.5)
    }

    S <- rbinom(n, 1, 0.8 - 0.6*W1 - 0.199*W2)

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

covered <- function(x) {
    c(dplyr::between(truth, x$confint[1], x$confint[2]),
      dplyr::between(truth, x$ipw_confint[1], x$ipw_confint[2]))
}

safe_sim <- possibly(function(n) {
    dat <- gendata(n)

    Np <- transport_Npsem$new(dat, c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    gamma1 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                             "gaussian", "adaptive-lasso")

    Np <- transport_Npsem$new(dat, c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    gamma2 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                              "gaussian", "adaptive-lasso-sl")

    Np <- transport_Npsem$new(dat, c("W1", "W2"), Z = "A", S = "S", Y = "Y")
    gamma3 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.xgboost"),
                                              "gaussian", "sl")

    data.frame(estimator = c("gamma1", "gamma1_ipw", "gamma2", "gamma2_ipw", "gamma3", "gamma3_ipw"),
               order = 1:6,
               psi = c(gamma1$theta, gamma1$ipw, gamma2$theta, gamma2$ipw, gamma3$theta, gamma3$ipw),
               var = c(gamma1$var, gamma1$ipw_var, gamma2$var, gamma2$ipw_var, gamma3$var, gamma3$ipw_var),
               covered = c(covered(gamma1)[1], covered(gamma1)[2], covered(gamma2)[1], covered(gamma2)[2], covered(gamma3)[1], covered(gamma3)[2]))
}, NULL)

plan(multisession)

res <- map_dfr(c(#`100` = 100,
                 `1000` = 1000),
                 #`1e4` = 1e4),
               function(n) {
    future_map_dfr(1:500, function(i) safe_sim(n), .id = "i")
}, .id = "n") |>
    mutate(n = as.numeric(n))

plan(sequential)

out <- group_by(res, n, estimator, order) |>
    filter(var < 100) |>
    summarise(absbias = abs(mean(psi) - truth),
              nvar = var(psi),
              coverage = mean(covered),
              estimvar = mean(var)) |>
    mutate(nvar = nvar * n) |>
    ungroup() |>
    arrange(n, order)

saveRDS(out, "_research/sim_incomplete_ate/results/dgp4_U_cv.rds")
