transport_ate_incomplete2 <- function(transport_Npsem, learner, family, folds = 20) {
    folded <- make_folds(transport_Npsem$data, folds)

    V <- transport_Npsem$var("V", data = TRUE)
    if (ncol(V) == 0) {
        V <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    Z <- transport_Npsem$var("Z", data = TRUE)
    if (ncol(V) == 0) {
        Z <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    a <- transport_Npsem$var("A", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_S <- vector("numeric", nrow(transport_Npsem$data))
    pred_A <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_a <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    f_VZS1 <- vector("numeric", nrow(transport_Npsem$data))
    f_V <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(S | Z)
        fit_S <- train(Z[t, , drop = FALSE],
                       transport_Npsem$var("S", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_St <- predict_from_fit(fit_S, Z[t, , drop = FALSE])
        pred_S[v] <- predict_from_fit(fit_S, Z[v, , drop = FALSE])

        # compute P(A | S, W)
        fit_A <- train(transport_Npsem$history("A", data = TRUE)[t, ],
                       transport_Npsem$var("A", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_At <- predict_from_fit(fit_A, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[t, ])
        pred_A[v] <- predict_from_fit(fit_A, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[v, ])

        # compute E(Y| S=1, A, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ][s[t] == 1, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t][s[t] == 1],
                       family,
                       learner,
                       10)

        pred_Y_at <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[t, ])
        pred_Y_1t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[t, ])
        pred_Y_0t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[t, ])

        pred_Y_a[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[v, ])

        pred_A_a <- a[t] * pred_At + (1 - pred_At) * (1 - a[t])

        # construct T_(O, P)
        tmp_T_OP <- ((2*a[t] - 1) / pred_A_a) * (y[t] - pred_Y_at) + pred_Y_1t - pred_Y_0t

        # compute f(V)
        fit_V <- train(V[t, , drop = FALSE][s[t] == 1, , drop = FALSE],
                       tmp_T_OP[s[t] == 1],
                       "gaussian",
                       learner,
                       10)

        f_Vt <- predict_from_fit(fit_V, V[t, , drop = FALSE])
        f_V[v] <- predict_from_fit(fit_V, V[v, , drop = FALSE])

        # E(f(V) | Z, S = 1)
        fit_fVZS1 <- train(Z[t, , drop = FALSE][s[t] == 1, , drop = FALSE], f_Vt[s[t] == 1], "gaussian", learner, 10)
        f_VZS1t <- predict_from_fit(fit_fVZS1, Z[t, , drop = FALSE])

        tmp_U_OP <- (s[t] / pred_St)*(f_Vt - f_VZS1t) + f_VZS1t

        fit_U <- train(Z[t, , drop = FALSE], tmp_U_OP, "gaussian", learner, 10)
        f_VZS1[v] <- predict_from_fit(fit_U, Z[v, , drop = FALSE])
    }

    theta_init <- mean(f_VZS1[s == 0])

    ipwa <- s * (a / pred_A - (1 - a) / (1 - pred_A))
    hs <- (1 - pred_S) / pred_S

    eic <- (ipwa*hs*(y - a*f_V - pred_Y_0) +
                ((s*(1 - pred_S)) / pred_S)*(f_V - f_VZS1) +
                (1 - s)*(f_VZS1 - theta_init)) /
        (1 - mean(s))

    theta <- theta_init + mean(eic)
    var <- var(eic)

    se <- sqrt(var) / sqrt(nrow(transport_Npsem$data))
    ci <- theta + c(-1, 1)*qnorm(0.975)*se

    list(theta = theta,
         var = var,
         confint = ci)
}
