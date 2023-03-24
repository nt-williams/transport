transport_ate_incomplete_sans_Z <- function(transport_Npsem, learner, family, interpretable = TRUE, folds = 20) {
    folded <- make_folds(transport_Npsem$data, folds)

    w <- transport_Npsem$var("W", data = TRUE)
    if (ncol(w) == 0) {
        w <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    a <- transport_Npsem$var("A", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_Z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    f_W <- vector("numeric", nrow(transport_Npsem$data))
    hsW <- vector("numeric", nrow(transport_Npsem$data))
    esW <- vector("numeric", nrow(transport_Npsem$data))
    k_W <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(Z | S, W)
        fit_Z <- train(transport_Npsem$history("A", data = TRUE)[t, ],
                       transport_Npsem$var("A", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       20)

        pred_Zt <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[t, ])
        pred_Z[v] <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("A", data = TRUE)[v, ])

        # compute E(Y| S=1, Z, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ][s[t] == 1, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t][s[t] == 1],
                       family,
                       learner,
                       20)

        pred_Y_zt <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[t, ])
        pred_Y_1t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[t, ])
        pred_Y_0t <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[t, ])

        pred_Y_z[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[v, ])

        pred_Z_z <- a[t] * pred_Zt + (1 - pred_Zt) * (1 - a[t])

        # construct T_(O, P)
        tmp_T_OP <- ((2*a[t] - 1) / pred_Z_z) * (y[t] - pred_Y_zt) + pred_Y_1t - pred_Y_0t

        if (interpretable) {
            penalty <- lm(tmp_T_OP ~ ., data = data.frame(tmp_T_OP = tmp_T_OP, w[t, ])[s[t] == 1, ])
            penalty <- 1 / abs(penalty$coefficients)

            if (ncol(w) == 1) {
                X <- model.matrix(~ ., as.data.frame(scale(w, T, F)))
                p <- penalty
            } else {
                X <- model.matrix(~ ., as.data.frame(scale(w, T, F)))[, -1]
                p <- penalty[-1]
            }

            fit_V <- glmnet::cv.glmnet(
                X[t, , drop = FALSE][s[t] == 1, , drop = FALSE],
                as.matrix(tmp_T_OP[s[t] == 1]),
                family = "gaussian", penalty.factor = p, nfolds = 20
            )

            V <- names(as.matrix(coef(fit_V, s = "lambda.min"))[which(as.matrix(coef(fit_V, s = "lambda.min")) != 0), ])[-1]
        }

        if (!interpretable) V <- names(w)

        if (is.null(V)) {
            X <- data.frame(X1 = rep(1, nrow(w)))
        } else {
            X <- w[, V, drop = FALSE]
        }

        fit_V <- train(X[t, , drop = FALSE][s[t] == 1, , drop = FALSE],
                       tmp_T_OP[s[t] == 1],
                       "gaussian",
                       learner,
                       20)

        f_Wt <- predict_from_fit(fit_V, X[t, , drop = FALSE])
        f_W[v] <- predict_from_fit(fit_V, X[v, , drop = FALSE])

        # compute P(S = 1|V), e_S(W)
        if (is.null(V)) {
            fit_eS <- glm(s ~ 1, data = data.frame(s = s, X)[t, , drop = F])
            esWt <- predict(fit_eS, data = X[t, , drop = F])
            esW[v] <- predict(fit_eS, data = X[v, , drop = F])
            z <- NULL
        } else {
            if (interpretable) {
                penalty <- lm(s ~ ., data = data.frame(s = s, X)[t, ])
                penalty <- 1 / abs(penalty$coefficients)

                if (ncol(X) == 1) {
                    Z <- model.matrix(~ ., as.data.frame(X))
                    p <- penalty
                } else {
                    Z <- model.matrix(~ ., as.data.frame(X))[, -1]
                    p <- penalty[-1]
                }

                fit_eS <- glmnet::cv.glmnet(
                    Z[t, , drop = FALSE],
                    as.matrix(s[t]),
                    family = "binomial", penalty.factor = p, nfolds = 20
                )

                z <- names(as.matrix(coef(fit_eS, s = "lambda.min"))[which(as.matrix(coef(fit_eS, s = "lambda.min")) != 0), ])[-1]
            }

            if (!interpretable) z <- names(w)

            if (is.null(z)) {
                Z <- data.frame(X1 = rep(1, nrow(w)))
            } else {
                Z <- w[, z, drop = FALSE]
            }

            fit_eS <- train(Z[t, , drop = FALSE], s[t], "binomial", learner, 20)
            esWt <- predict_from_fit(fit_eS, Z[t, , drop = FALSE])
            esW[v] <- predict_from_fit(fit_eS, Z[v, , drop = FALSE])
        }


        if (var(f_Wt) == 0) {
            fit_hsW <- glm(s ~ f_W, family = "binomial", data = data.frame(s = s[t], f_W = f_Wt))
            hsW[v] <- predict(fit_hsW, data.frame(f_W = f_W[v]), type = "response")
        } else {
            fit_hsW <- train(data.frame(f_W = f_Wt),
                             s[t],
                             "binomial",
                             learner,
                             20)
            hsW[v] <- predict_from_fit(fit_hsW, data.frame(f_W = f_W[v]))
        }

        if (var(esWt) == 0) {
            fit_K <- glm(f_W ~ esW, family = "gaussian", data = data.frame(esW = esWt, f_W = f_Wt))
            k_W[v] <- predict(fit_K, data.frame(esW = esW[v]))
        } else {
            fit_K <- train(data.frame(esW = esWt),
                           f_Wt,
                           "gaussian",
                           learner,
                           20)
            k_W[v] <- predict_from_fit(fit_K, data.frame(esW = esW[v]))
        }
    }

    lambda <- (1 / (nrow(transport_Npsem$data) * mean(s == 0))) * sum(k_W[s == 0])
    hs <- (1 - hsW) / hsW

    ipwz <- s / (1 - mean(s)) * (a / pred_Z - (1 - a) / (1 - pred_Z))

    eic <- ipwz * hs * (y - a*f_W - pred_Y_0) +
        ((1 - esW) / (1 - mean(s))) * (f_W - k_W) +
        ((1 - s) / (1 - mean(s))) * (k_W - lambda)

    theta <- lambda + mean(eic)
    var <- var(eic)

    se <- sqrt(var) / sqrt(nrow(transport_Npsem$data))
    ci <- theta + c(-1, 1)*qnorm(0.975)*se

    if (!interpretable & folds == 1) {
        V <- NA_character_
        z <- NA_character_
    }

    list(theta = theta,
         var = var,
         confint = ci,
         selected_V = V,
         selected_Z = z)
}
