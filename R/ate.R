ate <- function(transport_Npsem, learner, family, folds = 20) {
    s <- transport_Npsem$var("S", data = TRUE, drop = TRUE)
    transport_Npsem <- transport_Npsem$clone()
    transport_Npsem$data <- transport_Npsem$data[s == 1, ]
    transport_Npsem$S <- NULL

    folded <- make_folds(transport_Npsem$data, folds)

    a <- transport_Npsem$var("A", data = TRUE, drop = TRUE)
    y <- transport_Npsem$var("Y", data = TRUE, drop = TRUE)
    y[is.na(y)] <- -999

    pred_Z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_z <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_1 <- vector("numeric", nrow(transport_Npsem$data))
    pred_Y_0 <- vector("numeric", nrow(transport_Npsem$data))
    for (i in 1:folds) {
        t <- folded[[i]]$training_set
        v <- folded[[i]]$validation_set

        # compute P(Z | S=1, W)
        fit_Z <- train(transport_Npsem$history("A", data = TRUE)[t, , drop = FALSE],
                       transport_Npsem$var("A", data = TRUE, drop = TRUE)[t],
                       "binomial",
                       learner,
                       10)

        pred_Z[v] <- predict_from_fit(fit_Z, transport_Npsem$history("A", data = TRUE)[v, , drop = FALSE])

        # compute E(Y| Z, W)
        fit_Y <- train(transport_Npsem$history("Y", data = TRUE)[t, ],
                       transport_Npsem$var("Y", data = TRUE, drop = TRUE)[t],
                       family,
                       learner,
                       10)

        pred_Y_z[v] <- predict_from_fit(fit_Y, transport_Npsem$history("Y", data = TRUE)[v, ])
        pred_Y_1[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 1)$history("Y", data = TRUE)[v, ])
        pred_Y_0[v] <- predict_from_fit(fit_Y, transport_Npsem$modify("A", 0)$history("Y", data = TRUE)[v, ])
    }

    pred_Z_z <- a * pred_Z + (1 - pred_Z) * (1 - a)

    eic <- ((2*a - 1) / pred_Z_z) * (y - pred_Y_z) + pred_Y_1 - pred_Y_0
    theta <- mean(eic)
    var <- var(eic)

    se <- sqrt(var) / sqrt(nrow(transport_Npsem$data))
    ci <- theta + c(-1, 1)*qnorm(0.975)*se

    list(theta = theta,
         var = var,
         confint = ci)
}
