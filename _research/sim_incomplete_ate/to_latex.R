suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

dgp <- "dgp2"
cv <- F

if (cv) {
    dat <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}-cf.rds"))
    datU <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_TRUE.rds"))
} else {
    dat <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}.rds"))
    datU <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_FALSE.rds"))
}

if (dgp == "dgp3") dgp <- "dgp5"

filter(datU, estimator == "gamma2") |>
    mutate(releff = estimvar / filter(dat, estimator == "lambda", n != 500)$estimvar,
           order = 3) |>
    bind_rows(filter(dat, !endsWith(estimator, "_ipw"), n != 500)) |>
    arrange(n, order) |>
    select(estimator, n, absbias, coverage, nvar, estimvar, releff) |>
    mutate(estimator = case_when(estimator == "theta" ~ "$\\theta$",
                                 estimator == "lambda" ~ "$\\lambda$",
                                 estimator == "gamma2" ~ "c-$\\lambda$",
                                 TRUE ~ "c-$\\lambda$\\textsuperscript{a}")) |>
    kbl("latex",
        booktabs = TRUE,
        digits = 2,
        linesep = "",
        col.names = c("Parameter", "$n$", "$\\text{Bias}$", "95\\% CI Covr.", "$n \\times$ Var.", "Var.", "Rel. Eff."),
        escape = FALSE,
        align = "ccccccc") |>
    collapse_rows(columns = 2, latex_hline = "none", valign = "middle")
