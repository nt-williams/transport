suppressPackageStartupMessages({
    library(glue)
    library(transport)
    library(tidyverse)
})

dgp <- 2
crossfit <- T

case_when(dgp == 1 ~ source("_research/sim_incomplete_ate/dgp1.R"),
          dgp == 2 ~ source("_research/sim_incomplete_ate/dgp2.R"),
          dgp == 4 ~ source("_research/sim_incomplete_ate/dgp4.R"),
          dgp == 5 ~ source("_research/sim_incomplete_ate/dgp5.R"))

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

covered <- function(x) {
    c(between(truth, x$confint[1], x$confint[2]),
      between(truth, x$ipw_confint[1], x$ipw_confint[2]))
}

sim <- possibly(function(n) {
    dat <- gendata(n)

    if (crossfit) {
        folds <- case_when(n == 100 ~ 40,
                           n == 1000 ~ 20,
                           n == 1e4 ~ 10)
    } else {
        folds <- 1
    }

    if (dgp %in% c(1, 2)) {
        w <- c("W1")
    } else {
        w <- c("W1", "W2")
    }

    Np <- transport_Npsem$new(dat, w, Z = "A", S = "S", Y = "Y")
    gamma1 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                              "gaussian", "adaptive-lasso", folds)

    Np <- transport_Npsem$new(dat, w, Z = "A", S = "S", Y = "Y")
    gamma2 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean"),
                                              "gaussian", "adaptive-lasso-sl", folds)

    Np <- transport_Npsem$new(dat, w, Z = "A", S = "S", Y = "Y")
    gamma3 <- transport_ate_incomplete_sans_V(Np, c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.ranger"),
                                              "gaussian", "sl", folds)

    data.frame(estimator = c("gamma1", "gamma1_ipw", "gamma2", "gamma2_ipw", "gamma3", "gamma3_ipw"),
               order = 1:6,
               psi = c(gamma1$theta, gamma1$ipw, gamma2$theta, gamma2$ipw, gamma3$theta, gamma3$ipw),
               var = c(gamma1$var, gamma1$ipw_var, gamma2$var, gamma2$ipw_var, gamma3$var, gamma3$ipw_var),
               covered = c(covered(gamma1)[1], covered(gamma1)[2],
                           covered(gamma2)[1], covered(gamma2)[2],
                           covered(gamma3)[1], covered(gamma3)[2]))
}, NULL)

res <- map_dfr(c(`100` = 100, `1000` = 1000, `1e4` = 1e4),
               function(n) sim(n), .id = "n")
res <- mutate(res, n = as.numeric(n))

saveRDS(res, glue("_research/sim_incomplete_ate/results/raw/dgp{dgp}_U_{crossfit}_{id}.rds"))
