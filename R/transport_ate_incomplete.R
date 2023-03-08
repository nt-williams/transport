transport_ate_incomplete <- function(transport_Npsem, learner, family, folds = 20) {
    folded <- make_folds(transport_Npsem$data, folds)

    V <- transport_Npsem$var("V", data = TRUE)
    if (ncol(V) == 0) {
        V <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    a <- transport_Npsem$var("A", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_S <- vector("numeric", nrow(transport_Npsem$data))
    pred_Z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    f_V <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(S | V)
        fit_S <- train(V[t, , drop = FALSE],
                       transport_Npsem$var("S", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_S[v] <- predict_from_fit(fit_S, V[v, , drop = FALSE])

        # compute P(Z | S, W)
        fit_Z <- train(transport_Npsem$history("A", data = TRUE)[t, ],
                       transport_Npsem$var("A", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_Zt <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[t, ])
        pred_Z[v] <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[v, ])

        # compute E(Y| S=1, Z, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ][s[t] == 1, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t][s[t] == 1],
                       family,
                       learner,
                       10)

        pred_Y_zt <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[t, ])
        pred_Y_1t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[t, ])
        pred_Y_0t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[t, ])

        pred_Y_z[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[v, ])

        pred_Z_z <- a[t] * pred_Zt + (1 - pred_Zt) * (1 - a[t])

        # construct T_(O, P)
        tmp_T_OP <- ((2*a[t] - 1) / pred_Z_z) * (y[t] - pred_Y_zt) + pred_Y_1t - pred_Y_0t

        # compute f(V)
        fit_V <- train(V[t, , drop = FALSE][s[t] == 1, , drop = FALSE],
                       tmp_T_OP[s[t] == 1],
                       "gaussian",
                       learner,
                       10)

        f_V[v] <- predict_from_fit(fit_V, V[v, , drop = FALSE])
    }

    theta_init <- mean(f_V[s == 0])

    ipwz <- s / (1 - mean(s)) * (a / pred_Z - (1 - a) / (1 - pred_Z))
    hs <- (1 - pred_S) / pred_S

    eic <- ipwz * hs  * (y - a*f_V - pred_Y_0) + ((1 - s) / (1 - mean(s))) * (f_V - theta_init)

    ipw <- s / (1 - mean(s)) * (1 / pred_Z) * hs
    d.w <- survey::svydesign(~ 1, weights = ipw[s == 1], data = transport_Npsem$data[s == 1, ])
    f <- reformulate(transport_Npsem$A, transport_Npsem$Y)
    fit <- survey::svyglm(f, design = d.w, data = transport_Npsem$data[s == 1, ])

    theta <- theta_init + mean(eic)
    var <- var(eic)

    se <- sqrt(var) / sqrt(nrow(transport_Npsem$data))
    ci <- theta + c(-1, 1)*qnorm(0.975)*se

    list(theta = theta,
         var = var,
         confint = ci,
         ipw = as.numeric(coef(fit)[2]),
         ipw_var = summary(fit)$coefficients[2, 2]^2 * nrow(transport_Npsem$data),
         ipw_confint = as.numeric(confint(fit)[2, ]))
}
