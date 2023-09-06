eif_transport_ate <- function(data, trt, outcome, source, pi_trt, pi_src, m) {
    Y <- data[[outcome]]
    Y[is.na(Y)] <- -999
    S <- data[[source]]
    A <- data[[trt]]

    `1(S=1)` <- as.numeric(S == 1)
    `1(S=0)` <- as.numeric(S == 0)
    `P(S=0)` <- 1 - mean(S)
    `P(S=1|W)` <- pi_src[, 1]
    `P(S=0|W)` <- 1 - `P(S=1|W)`
    `P(A|S=1,W)` <- A*pi_trt[, 1] + (1 - pi_trt[, 1])*(1 - A)
    `f(W)` <- m[, 2] - m[, 1]
    `g(W)` <- m[, 1]

    lambda <- mean(`f(W)`[S == 0])

    eif <- (1 / `P(S=0)`)*(((`1(S=1)`*(2*A - 1)) / `P(A|S=1,W)`)*(`P(S=0|W)`/`P(S=1|W)`) *
                               (Y - A*`f(W)` - `g(W)`) + `1(S=0)`*(`f(W)` - lambda))

    list(theta = lambda + mean(eif),
         var = var(eif) / nrow(data),
         eif = eif)
}

eif_transport_ittate <- function() {

}

eif_transport_cace <- function() {

}
