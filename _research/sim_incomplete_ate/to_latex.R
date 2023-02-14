suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

dgp <- "dgp4"

dat <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}-cf.rds"))

if (dgp %in% c("dgp1", "dgp2")) {
    dat |>
        filter(!endsWith(estimator, "_ipw")) |>
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
}

if (dgp %in% c("dgp3", "dgp4")) {
    # datU <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_FALSE.rds"))
    if (dgp == "dgp3") dgp <- "dgp5"
    datUcv <- readRDS(glue("_research/sim_incomplete_ate/results/{dgp}_U_TRUE.rds"))

    filter(datUcv, estimator == "gamma2") |>
        mutate(releff = estimvar / filter(dat, estimator == "lambda", n != 500)$estimvar,
               order = 3) |>
        bind_rows(filter(dat, !endsWith(estimator, "_ipw"), n != 500)) |>
        # bind_rows({
        #     filter(datUcv, estimator == "gamma2") |>
        #         mutate(releff = estimvar / filter(dat, estimator == "lambda", n != 500)$estimvar,
        #                estimator = "gamma2cv",
        #                order = 4)
        # }) |>
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
}
