suppressPackageStartupMessages({
    library(glue)
    library(transport)
    library(tidyverse)
})

source("_research/sim_incomplete_ate/dgp2.R")

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)
crossfit <- as.logical(args[[1]])

covered <- function(x) between(truth, x$confint[1], x$confint[2])

sim <- possibly(function(n) {
    dat <- gendata(n)

    if (crossfit) {
        folds <- case_when(n == 100 ~ 40,
                           n == 1000 ~ 20,
                           n == 1e4 ~ 10)
    } else {
        folds <- 1
    }

    w <- "W1"

    Np <- transport_Npsem$new(dat, w, A = "A", S = "S", Y = "Y")
    lambda <- transport_ate(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)

    Np <- transport_Npsem$new(dat, w, V = NULL, A = "A", S = "S", Y = "Y")
    theta <- transport_ate_incomplete(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)

    # Np <- transport_Npsem$new(dat, w, V = v, Z = z, A = "A", S = "S", Y = "Y")
    # theta1 <- transport_ate_incomplete1(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)
    # theta2 <- transport_ate_incomplete2(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"), "gaussian", folds)

    Np <- transport_Npsem$new(dat, w, A = "A", S = "S", Y = "Y")
    clambda1 <- transport_ate_incomplete_sans_Z(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                                "gaussian", T, folds)

    clambda2 <- transport_ate_incomplete_sans_Z(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                                "gaussian", F, folds)

    # Np <- transport_Npsem$new(dat, w, A = "A", S = "S", Y = "Y")
    # clambda1 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
    #                                             "gaussian", "adaptive-lasso", folds)
    #
    # Np <- transport_Npsem$new(dat, w, A = "A", S = "S", Y = "Y")
    # clambda2 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
    #                                             "gaussian", "adaptive-lasso-sl", folds)
    #
    # Np <- transport_Npsem$new(dat, w, A = "A", S = "S", Y = "Y")
    # clambda3 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.lightgbm"),
    #                                             "gaussian", "sl", folds)

    data.frame(estimator = c("lambda", "theta", "clambda1", "clambda2"),
               order = 1:4,
               psi = c(lambda$theta, theta$theta, clambda1$theta, clambda2$theta),
               var = c(lambda$var, theta$var, clambda1$var, clambda2$var),
               covered = map_lgl(list(lambda, theta, clambda1, clambda2), covered))
}, NULL)

res <- map_dfr(c(`100` = 100, `1000` = 1000, `1e4` = 1e4),
               function(n) sim(n), .id = "n")
res <- mutate(res, n = as.numeric(n))

saveRDS(res, glue("_research/sim_incomplete_ate/results/raw/dgp2_{crossfit}_{id}.rds"))
