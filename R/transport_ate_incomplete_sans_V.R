transport_ate_incomplete_sans_V <- function(transport_Npsem, learner, family, folds = 1) {
    w <- transport_Npsem$var("W", data = TRUE)
    if (ncol(w) == 0) {
        w <- data.frame(X = rep(1, nrow(transport_Npsem$data)))
    }

    # compute P(Z | S, W)
    fit_Z <- train(transport_Npsem$history("Z", data = TRUE),
                   transport_Npsem$var("Z", data = TRUE, drop = TRUE),
                   "binomial",
                   learner,
                   10)

    pred_Z <- predict_from_fit(fit_Z, transport_Npsem$modify("S", 1)$history("Z", data = TRUE))

    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)

    # compute E(Y| S=1, Z, W)
    fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[s == 1, ],
                   transport_Npsem$var("Y", data = TRUE, drop = TRUE)[s == 1],
                   family,
                   learner,
                   10)

    pred_Y_z <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE))
    pred_Y_1 <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 1)$history("Y", data = TRUE))
    pred_Y_0 <- predict_from_fit(fit_Y, transport_Npsem$modify("Z", 0)$history("Y", data = TRUE))

    a <- transport_Npsem$var("Z", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999
    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    # construct T_(O, P)
    tmp_T_OP <- ((2*a - 1) / pred_Z_z) * (y - pred_Y_z) + pred_Y_1 - pred_Y_0

    penalty <- lm(tmp_T_OP ~ ., data = data.frame(tmp_T_OP = tmp_T_OP, w)[s == 1, ])
    penalty <- 1 / abs(penalty$coefficients)

    fit_V <- glmnet::cv.glmnet(
        model.matrix(~ ., w[s == 1, ])[, -1],
        as.matrix(tmp_T_OP[s == 1]),
        family = "gaussian", penalty.factor = penalty[-1], nfolds = 20
    )

    selected <- coef(fit_V, s = "lambda.min")

    f_W <- predict(fit_V, s = "lambda.min", gamma = c(1),
                   relax = TRUE, newx = model.matrix(~ ., w)[, -1])[, 1]

    lambda <- (1 / (nrow(transport_Npsem$data) * mean(s == 0))) * sum(f_W[s == 0])

    fit_hsW <- glm(s ~ f_W, family = "binomial", data = data.frame(s = s, f_W = f_W))
    hsW <- predict(fit_hsW, type = "response")
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

    list(theta = theta,
         var = var,
         confint = ci,
         ipw = as.numeric(coef(fit)[2]),
         ipw_var = summary(fit)$coefficients[2, 2]^2 * nrow(transport_Npsem$data),
         ipw_confint = as.numeric(confint(fit)[2, ]))
}
