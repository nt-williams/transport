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

eif_transport_ittate <- function(data, instrument, trt, outcome, source,
                                 pi_I1, pi_I0, pi_trt0, pi_trt1, pi_src, m, m_pseudo) {
    Y <- data[[outcome]]
    Y[is.na(Y)] <- -999
    S <- data[[source]]
    A <- data[[instrument]]
    Z <- data[[trt]]

    `1(S=1)` <- as.numeric(S == 1)
    `1(S=0)` <- as.numeric(S == 0)
    `P(S=1)` <- mean(S)
    `P(S=0)` <- 1 - `P(S=1)`
    `P(S=1|W)` <- pi_src[, 1]
    `P(S=0|W)` <- 1 - `P(S=1|W)`

    pmeif <- function(a) {
        `1(A=a)` <- as.numeric(A == a)
        `Q(1,W,a,Z)` <- m[, a + 2]
        `E[Q(1,W,a,Z)|S=0,W,a]` <- m_pseudo[[a + 2]][, 1]
        `P(A=a|S=1,W)` <- a*pi_I1[, 1] + (1 - pi_I1[, 1])*(1 - a)
        `P(A=a|S=0,W)` <- a*pi_I0[, 1] + (1 - pi_I0[, 1])*(1 - a)
        `P(Z=z|a,S=1,W)` <- Z*pi_trt1[, a + 1] + (1 - Z)*(1 - pi_trt1[, a + 1])
        `P(Z=z|a,S=0,W)` <- Z*pi_trt0[, a + 1] + (1 - Z)*(1 - pi_trt0[, a + 1])

        lambda <- mean(`E[Q(1,W,a,Z)|S=0,W,a]`[S == 0])

        D_w <- (`1(S=0)` / `P(S=0)`)*(`E[Q(1,W,a,Z)|S=0,W,a]` - lambda)
        D_z <- ((`1(A=a)`/`P(A=a|S=0,W)`)*(`1(S=0)`/`P(S=0)`))*(`Q(1,W,a,Z)` - `E[Q(1,W,a,Z)|S=0,W,a]`)
        D_y <- ((`1(A=a)`/`P(A=a|S=1,W)`)*(`1(S=1)`/`P(S=1)`)*(`P(Z=z|a,S=0,W)`/`P(Z=z|a,S=1,W)`)*((`P(S=0|W)`*`P(S=1)`)/(`P(S=1|W)`*`P(S=0)`)))*(Y - `Q(1,W,a,Z)`)

        eif <- D_y + D_z + D_w

        list(theta = lambda + mean(eif),
             eif = eif)
    }

    est <- lapply(c(0, 1), pmeif)

    list(theta = est[[2]]$theta - est[[1]]$theta,
         var = var(est[[2]]$eif - est[[1]]$eif) / nrow(data),
         eif = est[[2]]$eif - est[[1]]$eif)

}

eif_transport_cace <- function(data, instrument, trt, source, pi_I0, pi_trt0, eif_ittate) {
    browser()
    S <- data[[source]]
    A <- data[[instrument]]
    Z <- data[[trt]]

    `1(S=0)` <- as.numeric(S == 0)
    `P(S=1)` <- mean(S)
    `P(S=0)` <- 1 - `P(S=1)`
    `P(A=1|S=0,W)` <- pi_I0[, 1]
    `P(A=0|S=0,W)` <- 1 - `P(A=1|S=0,W)`
    `P(Z=1|A=0,S=0,W)` <- pi_trt0[, 1]
    `P(Z=1|A=1,S=0,W)` <- pi_trt0[, 2]
    `P(Z=1|A=a,S=0,W)` <- A*`P(Z=1|A=1,S=0,W)` + (1 - A)*`P(Z=1|A=0,S=0,W)`

    theta1 <- mean((`P(Z=1|A=0,S=0,W)` - `P(Z=1|A=1,S=0,W)`)[S == 0])

    h0w <- `1(S=0)` * (1 - A) / (`P(A=0|S=0,W)`*`P(S=0)`)
    h1w <- `1(S=0)` * A / (`P(A=1|S=0,W)`*`P(S=0)`)

    eif_ate <- (((A * h1w) - ((1 - A) * h0w)) * (Z - `P(Z=1|A=a,S=0,W)`)) +
        ((`1(S=0)` / `P(S=0)`) * ((`P(Z=1|A=1,S=0,W)` - `P(Z=1|A=0,S=0,W)`) - theta1))

    theta_ate <- theta1 + mean(eif_ate)

    eif <- (eif_ittate$eif / theta_ate) - (eif_ittate$theta / (theta_ate^2)) * eif_ate

    list(theta = eif_ittate$theta / theta_ate,
         var = var(eif) / nrow(data),
         eif = eif)
}
