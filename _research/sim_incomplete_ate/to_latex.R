suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

dgp <- "dgp4"

dat <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}.rds"))

select(dat, estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    filter(!endsWith(estimator, "_ipw")) |>
    mutate(estimator = if_else(estimator == "theta", "$\\theta$", "$\\lambda$")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")

select(dat, estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    filter(endsWith(estimator, "_ipw")) |>
    mutate(estimator = if_else(estimator == "theta_ipw", "$\\theta_{\\text{IPW}}$", "$\\lambda_{\\text{IPW}}$")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")
