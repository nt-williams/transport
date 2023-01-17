transport_ate <- function(transport_Npsem, learner, family) {
    # compute P(S | V)
    fit_S <- train(transport_Npsem$var("W", data = TRUE),
                   transport_Npsem$var("S", data = TRUE, drop = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_S <- predict_from_fit(fit_S, transport_Npsem$var("W", data = TRUE))

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

    lambda <- mean((pred_Y_1 - pred_Y_0)[s == 0])

    a <- transport_Npsem$var("Z", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    eic <- ifelse(s == 1,
           (((2*a - 1) * s) / pred_Z_z) * ((1 - pred_S) / (pred_S * mean(s))) *
               (y - pred_Y_z),
           0) +
        ((1 - pred_S) / mean(s)) * (pred_Y_1 - pred_Y_0 - lambda)

    list(theta = lambda + mean(eic),
         var = var(eic))
}
