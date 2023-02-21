read_zip <- function(tar) {
    files <- unzip(tar, list = TRUE)$Name
    p <- progressr::progressor(along = 1:length(files))
    purrr::map(files, function(file) {
        p()
        con <- gzcon(unz(tar, file))
        x <- readRDS(con)
        close(con)
        x
    })
}

dgp <- 2
cv <- T

case_when(dgp == 1 ~ source("_research/sim_incomplete_ate/dgp1.R"),
          dgp == 2 ~ source("_research/sim_incomplete_ate/dgp2.R"),
          dgp == 4 ~ source("_research/sim_incomplete_ate/dgp4.R"),
          dgp == 5 ~ source("_research/sim_incomplete_ate/dgp5.R"))

dgp <- glue("dgp{dgp}_U_{cv}")

res <- read_zip(glue("_research/sim_incomplete_ate/results/raw/{dgp}.zip")) |>
    bind_rows()

res <- filter(res, abs(psi) < 10)

out <- group_by(res, n, estimator, order) |>
    summarise(absbias = abs(mean(psi) - truth),
              nvar = var(psi),
              coverage = mean(covered),
              estimvar = mean(var)) |>
    mutate(nvar = nvar * n) |>
    ungroup() |>
    arrange(n, order)

saveRDS(out, glue("_research/sim_incomplete_ate/results/{dgp}.rds"))
