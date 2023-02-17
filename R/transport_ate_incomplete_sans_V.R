transport_ate_incomplete_sans_V <- function(transport_Npsem, learner, family,
                                            method = c("adaptive-lasso", "adaptive-lasso-sl", "sl"),
                                            folds = 20) {
    method <- match.arg(method)
    folded <- make_folds(transport_Npsem$data, folds)

    w <- transport_Npsem$var("W", data = TRUE)
    if (ncol(w) == 0) {
        w <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    a <- transport_Npsem$var("Z", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_Z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    f_W <- vector("numeric", nrow(transport_Npsem$data))
    hsW <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(Z | S, W)
        fit_Z <- train(transport_Npsem$history("Z", data = TRUE)[t, ],
                       transport_Npsem$var("Z", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_Zt <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("Z", data = TRUE)[t, ])
        pred_Z[v] <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("Z", data = TRUE)[v, ])

        # compute E(Y| S=1, Z, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ][s[t] == 1, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t][s[t] == 1],
                       family,
                       learner,
                       10)

        pred_Y_zt <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[t, ])
        pred_Y_1t <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 1)$history("Y", data = TRUE)[t, ])
        pred_Y_0t <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 0)$history("Y", data = TRUE)[t, ])

        pred_Y_z[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 0)$history("Y", data = TRUE)[v, ])

        pred_Z_z <- a[t] * pred_Zt + (1 - pred_Zt) * (1 - a[t])

        # construct T_(O, P)
        tmp_T_OP <- ((2*a[t] - 1) / pred_Z_z) * (y[t] - pred_Y_zt) + pred_Y_1t - pred_Y_0t

        if (method %in% c("adaptive-lasso", "adaptive-lasso-sl")) {
            penalty <- lm(tmp_T_OP ~ ., data = data.frame(tmp_T_OP = tmp_T_OP, w[t, ])[s[t] == 1, ])
            penalty <- 1 / abs(penalty$coefficients)

            fit_V <- glmnet::cv.glmnet(
                model.matrix(~ ., as.data.frame(scale(w, T, F)[t, ][s[t] == 1, ]))[, -1],
                as.matrix(tmp_T_OP[s[t] == 1]),
                family = "gaussian", penalty.factor = penalty[-1], nfolds = 20
            )

            V <- names(as.matrix(coef(fit_V, s = "lambda.min"))[which(as.matrix(coef(fit_V, s = "lambda.min")) != 0), ])[-1]

            if (method == "adaptive-lasso-sl") {
                # penalty2 <- glm(S ~ ., data = data.frame(S = s, w[t, ])[t, ], family = "binomial")
                # penalty2 <- 1 / abs(penalty2$coefficients)

                fit_V2 <- glmnet::cv.glmnet(
                    model.matrix(~ ., as.data.frame(scale(w, T, F)[t, ]))[, -1],
                    as.matrix(s),
                    family = "binomial"#, penalty.factor = penalty2[-1], nfolds = 20
                )

                V2 <- names(as.matrix(coef(fit_V2, s = "lambda.min"))[which(as.matrix(coef(fit_V2, s = "lambda.min")) != 0), ])[-1]
                V <- V[V %in% V2]
            }
        }

        if (method == "adaptive-lasso") {
            f_Wt <- predict(fit_V, s = "lambda.min", gamma = c(1),
                                relax = TRUE, newx = scale(model.matrix(~ ., w)[, -1], T, F)[t, ])[, 1]
            f_W[v] <- predict(fit_V, s = "lambda.min", gamma = c(1),
                           relax = TRUE, newx = scale(model.matrix(~ ., w)[, -1], T, F)[v, ])[, 1]
        }

        if (method != "adaptive-lasso") {
            if (method == "sl") V <- names(w)

            if (is.null(V)) learner <- "SL.mean"

            if (is.null(V)) {
                X <- data.frame(X1 = rep(1, nrow(w)))
            } else {
                X <- w[, V, drop = FALSE]
            }

            fit_V <- train(X[t, , drop = FALSE][s[t] == 1, , drop = FALSE],
                           tmp_T_OP[s[t] == 1],
                           "gaussian",
                           learner,
                           10)

            f_Wt <- predict_from_fit(fit_V, X[t, , drop = FALSE])
            f_W[v] <- predict_from_fit(fit_V, X[v, , drop = FALSE])
        }

        if (var(f_Wt) == 0) {
            fit_hsW <- glm(s ~ f_W, family = "binomial", data = data.frame(s = s[t], f_W = f_Wt))
            hsW[v] <- predict(fit_hsW, data.frame(f_W = f_W[v]), type = "response")
        } else {
            # fit_hsW <- hal9001::fit_hal(as.matrix(f_Wt), s[t], family = "binomial")
            # hsW[v] <- predict(fit_hsW, as.matrix(f_W[v]))

            # fit_hsW <- glm(s ~ f_W, family = "binomial", data = data.frame(s = s[t], f_W = f_Wt))
            # hsW[v] <- predict(fit_hsW, data.frame(f_W = f_W[v]), type = "response")

            fit_hsW <- train(data.frame(f_W = f_Wt),
                             s[t],
                             "binomial",
                             learner,
                             10)
            hsW[v] <- predict_from_fit(fit_hsW, data.frame(f_W = f_W[v]))
        }
    }

    lambda <- (1 / (nrow(transport_Npsem$data) * mean(s == 0))) * sum(f_W[s == 0])
    hs <- (1 - hsW) / hsW

    ipwz <- s / (1 - mean(s)) * (a / pred_Z - (1 - a) / (1 - pred_Z))

    eic <- ipwz * hs * (y - a*f_W - pred_Y_0) + ((1 - s) / (1 - mean(s))) * (f_W - lambda)

    ipw <- s / (1 - mean(s)) * (1 / pred_Z) * hs
    d.w <- survey::svydesign(~ 1, weights = ipw[s == 1], data = transport_Npsem$data[s == 1, ])
    f <- reformulate(transport_Npsem$Z, transport_Npsem$Y)
    fit <- survey::svyglm(f, design = d.w, data = transport_Npsem$data[s == 1, ])

    theta <- lambda + mean(eic)
    var <- var(eic)

    se <- sqrt(var) / sqrt(nrow(transport_Npsem$data))
    ci <- theta + c(-1, 1)*qnorm(0.975)*se

    if (match.arg(method) == "sl" || folds > 1) {
        V <- NA_character_
    }

    list(theta = theta,
         var = var,
         confint = ci,
         selected = V,
         ipw = as.numeric(coef(fit)[2]),
         ipw_var = summary(fit)$coefficients[2, 2]^2 * nrow(transport_Npsem$data),
         ipw_confint = as.numeric(confint(fit)[2, ]))
}
