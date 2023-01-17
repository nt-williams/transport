transport_ate_incomplete <- function(transport_Npsem, learner, family) {
    v <- transport_Npsem$var("V", data = TRUE)
    if (ncol(v) == 0) {
        v <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    # compute P(S | V)
    fit_S <- train(v,
                   transport_Npsem$var("S", data = TRUE, drop = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_S <- predict_from_fit(fit_S, v)

    # compute P(Z | S, W)
    fit_Z <- train(transport_Npsem$history("Z", data = TRUE),
                   transport_Npsem$var("Z", data = TRUE, drop = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_Z <- predict_from_fit(fit_Z,
                               transport_Npsem$modify("S", 1)$
                                   history("Z", data = TRUE))

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)

    # compute E(Y| S=1, Z, W)
    fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[s == 1, ],
                   transport_Npsem$var("Y", data = TRUE, drop = TRUE)[s == 1],
                   family,
                   learner,
                   10)

    pred_Y_z <- predict_from_fit(fit_Y, transport_Npsem$
                                     history("Y", data = TRUE))
    pred_Y_1 <- predict_from_fit(fit_Y,
                                 transport_Npsem$modify("Z", 1)$
                                     history("Y", data = TRUE))
    pred_Y_0 <- predict_from_fit(fit_Y,
                                 transport_Npsem$modify("Z", 0)$
                                     history("Y", data = TRUE))

    a <- transport_Npsem$var("Z", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    # construct T_(O, P)
    tmp_T_OP <- ((2*a - 1) / pred_Z_z) * (y - pred_Y_z) + pred_Y_1 - pred_Y_0

    # compute f(V)
    fit_V <- train(v[s == 1, , drop = FALSE],
                   tmp_T_OP[s == 1],
                   "gaussian",
                   learner,
                   10)

    f_V <- predict_from_fit(fit_V, v)

    theta_init <- mean(f_V[s == 0])

    eic <- ifelse(s == 1,
                  ((s * (2*a - 1)) / pred_Z_z) * ((1 - pred_S) / (pred_S * mean(s))) *
                      (y - a*f_V - pred_Y_0),
                  0) +
        (((1 - pred_S) / mean(s)) * (f_V - theta_init))

    list(theta = theta_init + mean(eic),
         var = var(eic))
}
