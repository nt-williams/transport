suppressPackageStartupMessages(library(tidyverse))

# Results in lambda variance larger than theta variance
gendata7 <- function(n, A = NULL) {
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

truth <- mean(subset(gendata7(1e7, 1), S == 0)$Yi) -
    mean(subset(gendata7(1e7, 0), S == 0)$Yi)

mean(subset(gendata7(1e7, 1), S == 1)$Yi) -
    mean(subset(gendata7(1e7, 0), S == 1)$Yi)

res <- map(1:500, function(x) {
    dat <- gendata7(1000)
    out <- vector("list", 2)
    names(out) <- c("lambda", "theta")

    Np <- transport_Npsem$new(dat, c("W1"), Z = "A", S = "S", Y = "Y")
    out[["lambda"]] <- transport_ate(Np, c("SL.glm", "SL.mean"), "gaussian")

    Np <- transport_Npsem$new(dat, c("W1"), V = NULL, Z = "A", S = "S", Y = "Y")
    out[["theta"]] <- transport_ate_incomplete(Np, c("SL.glm", "SL.mean"), "gaussian")
    out
})

hist(map_dbl(res, \(x) x$lambda$theta))
hist(map_dbl(res, \(x) x$theta$theta))

var(map_dbl(res, \(x) x$lambda$theta)) * 1000
var(map_dbl(res, \(x) x$theta$theta)) * 1000

mean(map_dbl(res, \(x) x$lambda$var))
mean(map_dbl(res, \(x) x$theta$var))

covered <- function(x, n) {
    se <- sqrt(x$var) / sqrt(n)
    ci <- x$theta + c(-1, 1)*qnorm(0.975)*se
    dplyr::between(truth, ci[1], ci[2])
}

mean(map_lgl(res, \(x) covered(x$lambda, n = 1000)))
mean(map_lgl(res, \(x) covered(x$theta, n = 1000)))
