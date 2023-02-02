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

dgp <- "dgp4_U_FALSE"

res <- read_zip(glue("_research/sim_incomplete_ate/results/raw/{dgp}.zip")) |>
    bind_rows()

res <- filter(res, abs(psi) < 100)

out <- group_by(res, n, estimator, order) |>
    summarise(absbias = abs(mean(psi) - truth),
              nvar = var(psi),
              coverage = mean(covered),
              estimvar = mean(var)) |>
    mutate(nvar = nvar * n) |>
    ungroup() |>
    arrange(n, order)

saveRDS(out, glue("_research/sim_incomplete_ate/results/{dgp}.rds"))
