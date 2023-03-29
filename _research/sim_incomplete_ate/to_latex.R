suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

dgp <- 4
cv <- T

res <- readRDS(glue("_research/sim_incomplete_ate/results/dgp{dgp}_{cv}.rds"))

filter(res, estimator %in% c("lambda", "theta1", "clambda1")) |>
    mutate(releff = estimvar / rep(filter(res, estimator == "lambda")$estimvar, each = 3)) |>
    select(estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    mutate(estimator = case_when(estimator == "theta" ~ "$\\gamma$",
                                 estimator == "lambda" ~ "$\\lambda$",
                                 estimator == "theta1" ~ "$\\theta$",
                                 estimator == "clambda1" ~ "c-$\\lambda$")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")
