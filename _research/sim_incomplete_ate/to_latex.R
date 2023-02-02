suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

dgp <- "dgp4"

dat <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}.rds"))
datU <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_FALSE.rds"))
datUcv <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_TRUE.rds"))

filter(datU, estimator == "gamma1") |>
    mutate(releff = estimvar / filter(dat, estimator == "lambda", n != 500)$estimvar,
           order = 3) |>
    bind_rows(filter(dat, !endsWith(estimator, "_ipw"), n != 500)) |>
    bind_rows({
        filter(datUcv, estimator == "gamma1") |>
            mutate(releff = estimvar / filter(dat, estimator == "lambda", n != 500)$estimvar,
                   estimator = "gamma1cv",
                   order = 4)
    }) |>
    arrange(n, order) |>
    select(estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    mutate(estimator = case_when(estimator == "theta" ~ "$\\theta$",
                                 estimator == "lambda" ~ "$\\lambda$",
                                 TRUE ~ "$\\gamma$")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")

filter(datU, estimator == "gamma1_ipw") |>
    mutate(releff = estimvar / filter(dat, estimator == "lambda_ipw", n != 500)$estimvar,
           order = 5) |>
    bind_rows(filter(dat, endsWith(estimator, "_ipw"), n != 500)) |>
    arrange(n, order) |>
    select(estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    mutate(estimator = case_when(estimator == "theta_ipw" ~ "$\\theta$",
                                 estimator == "lambda_ipw" ~ "$\\lambda$",
                                 TRUE ~ "$\\gamma$")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")
