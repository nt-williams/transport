transport_ate_incomplete <- function(transport_Npsem, learner, family) {
    # compute P(S | V)
    fit_S <- train(transport_Npsem$var("W", data = TRUE),
                   transport_Npsem$var("S", data = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_S <- predict_from_fit(fit_S, transport_Npsem$var("W", data = TRUE))

    # compute P(Z | S, W)
    fit_Z <- train(transport_Npsem$history("Z", data = TRUE),
                   transport_Npsem$var("Z", data = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_Z <- predict_from_fit(fit_Z,
                               transport_Npsem$modify("S", 1)$
                                   history("Z", data = TRUE))

    s <- transport_Npsem$var("S", data = TRUE)

    # compute E(Y| S=1, Z, W)
    fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[s == 1, ],
                   transport_Npsem$var("Y", data = TRUE)[s == 1],
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

    a <- transport_Npsem$var("Z", data = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE)
    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    # construct T_(O, P)
    tmp_T_OP <- ((2*a - 1) / pred_Z_z) * (y - pred_Y_z) + pred_Y_1 - pred_Y_0

    # compute f(V)
    fit_V <- train(transport_Npsem$var("V", data = TRUE)[s == 1, , drop = FALSE],
                   tmp_T_OP[s == 1],
                   "gaussian",
                   learner,
                   10)

    f_V <- predict_from_fit(fit_V, transport_Npsem$var("V", data = TRUE))
    # f_V <- plogis(log(0.2) + log(1.66)*transport_Npsem$data$W1)

    theta_init <- mean(f_V[s == 0])

    eic <- ifelse(s == 1,
                  ((s * (2*a - 1)) / pred_Z_z) * ((1 - pred_S) / (pred_S * mean(s))) *
                      (y - a*f_V - pred_Y_0),
                  0) +
        (((1 - pred_S) / mean(s)) * (f_V - theta_init))

    list(theta = theta_init + mean(eic),
         var = var(eic))
}
