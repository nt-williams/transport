transport_ate <- function(transport_Npsem, learner, family, folds = 20) {
    folded <- make_folds(transport_Npsem$data, folds)

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    a <- transport_Npsem$var("A", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_S <- vector("numeric", nrow(transport_Npsem$data))
    pred_Z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(S | W)
        fit_S <- train(transport_Npsem$var("W", data = TRUE)[t, , drop = FALSE],
                       transport_Npsem$var("S", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_S[v] <- predict_from_fit(fit_S, transport_Npsem$var("W", data = TRUE)[v, , drop = FALSE])

        # compute P(Z | S, W)
        fit_Z <- train(transport_Npsem$history("A", data = TRUE)[t, ],
                       transport_Npsem$var("A", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_Z[v] <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[v, ])

        # compute E(Y| S=1, Z, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ][s[t] == 1, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t][s[t] == 1],
                       family,
                       learner,
                       10)

        pred_Y_z[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[v, ])
    }

    lambda <- mean((pred_Y_1 - pred_Y_0)[s == 0])

    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    ipwz <- s / (1 - mean(s)) * (a / pred_Z - (1 - a) / (1 - pred_Z))
    hs <- (1 - pred_S) / pred_S

    eic <- ipwz * hs * (y - pred_Y_z) + ((1 - s) / (1 - mean(s))) * (pred_Y_1 - pred_Y_0 - lambda)

    ipw <- s / (1 - mean(s)) * (1 / pred_Z) * hs
    d.w <- survey::svydesign(~ 1, weights = ipw[s == 1], data = transport_Npsem$data[s == 1, ])
    f <- reformulate(transport_Npsem$A, transport_Npsem$Y)
    fit <- survey::svyglm(f, design = d.w, data = transport_Npsem$data[s == 1, ])

    theta <- lambda + mean(eic)
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
