suppressPackageStartupMessages({
    library(glue)
    library(transport)
    library(tidyverse)
    library(furrr)
})

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

truth <- mean(subset(gendata(1e7, 1), S == 0)$Yi) -
    mean(subset(gendata(1e7, 0), S == 0)$Yi)

covered <- function(x) {
    c(dplyr::between(truth, x$confint[1], x$confint[2]),
      dplyr::between(truth, x$ipw_confint[1], x$ipw_confint[2]))
}

safe_sim <- possibly(function(n) {
    dat <- gendata(n)

    folds <- case_when(n == 100 ~ 40,
                       n == 1000 ~ 20,
                       n == 1e4 ~ 10)

    Np <- transport_Npsem$new(dat, c("W1"), Z = "A", S = "S", Y = "Y")
    lambda <- transport_ate(Np, c("SL.glm", "SL.mean"), "gaussian")

    Np <- transport_Npsem$new(dat, c("W1"), V = NULL, Z = "A", S = "S", Y = "Y")
    theta <- transport_ate_incomplete(Np, c("SL.glm", "SL.mean"), "gaussian")

    data.frame(estimator = c("lambda", "theta", "lambda_ipw", "theta_ipw"),
               order = 1:4,
               psi = c(lambda$theta, theta$theta, lambda$ipw, theta$ipw),
               var = c(lambda$var, theta$var, lambda$ipw_var, theta$ipw_var),
               covered = c(covered(lambda)[1], covered(theta)[1], covered(lambda)[2],  covered(theta)[2]))
}, NULL)

plan(multisession)

res <- map_dfr(c(`100` = 100, `1000` = 1000, `1e4` = 1e4), function(n) {
    future_map_dfr(1:500, function(i) safe_sim(n), .id = "i")
}, .id = "n") |>
    mutate(n = as.numeric(n))

plan(sequential)

res <- filter(res, abs(psi) < 10)

out <- group_by(res, n, estimator, order) |>
    summarise(absbias = abs(mean(psi) - truth),
              nvar = var(psi),
              coverage = mean(covered),
              estimvar = mean(var)) |>
    mutate(nvar = nvar * n) |>
    ungroup() |>
    arrange(n, order)

ref <- rep(filter(out, startsWith(estimator, "lambda"))$estimvar, each = 2)
out <- mutate(out, releff = estimvar / ref)

saveRDS(out, "_research/sim_incomplete_ate/results/dgp2-cf.rds")
