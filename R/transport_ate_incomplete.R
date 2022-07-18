transport_ate_incomplete <- function(transport_Npsem, learners, family, ...) {
    # P(S | V)
    fit_S <- regress(transport_Npsem$var("V"), transport_Npsem$var("S"), learners, "binomial", 10)
    pred_S <- regress_predict(fit_S, transport_Npsem$data)

    # P(A | S, W)
    fit_A <- regress(transport_Npsem$history("A"), transport_Npsem$var("A"), learners, "binomial", 10)
    pred_A <- regress_predict(fit_A, transport_Npsem$modify("S", 1))

    # E(Y| S=1, A, W)
    fit_Y <- regress(transport_Npsem$history("Y")[transport_Npsem$var("S") == 1, ],
                     transport_Npsem$var("Y")[transport_Npsem$var("S") == 1],
                     learners, family, 10)
    pred_Y_a <- regress_predict(fit_Y, transport_Npsem$data)
    pred_Y_1 <- regress_predict(fit_Y, transport_Npsem$modify("A", 1))
    pred_Y_0 <- regress_predict(fit_Y, transport_Npsem$modify("A", 0))

    # E(E(Y| S=1, A = a, W) | S=1, V)
    fit_Pseudo_1 <- regress(transport_Npsem$data[, c(transport_Npsem$S, transport_Npsem$V)],
                            pred_Y_1, learners, "continuous", 10)
    pred_Pseudo_1 <- regress_predict(fit_Pseudo_1, transport_Npsem$modify("S", 1))

    fit_Pseudo_0 <- regress(transport_Npsem$data[, c(transport_Npsem$S, transport_Npsem$V)],
                            pred_Y_0, learners, "continuous", 10)
    pred_Pseudo_0 <- regress_predict(fit_Pseudo_0, transport_Npsem$modify("S", 1))

    # Influence function
    eif_1 <- transport_ate_incomplete_eif(
        transport_Npsem,
        list(
            pred_S = pred_S,
            pred_A = pred_A,
            pred_Y_a = pred_Y_a,
            pred_Y_1 = pred_Y_1,
            pred_Y_0 = pred_Y_0,
            pred_Pseudo_0 = pred_Pseudo_0,
            pred_Pseudo_1 = pred_Pseudo_1
        ), 1
    )

    eif_0 <- transport_ate_incomplete_eif(
        transport_Npsem,
        list(
            pred_S = pred_S,
            pred_A = pred_A,
            pred_Y_a = pred_Y_a,
            pred_Y_1 = pred_Y_1,
            pred_Y_0 = pred_Y_0,
            pred_Pseudo_0 = pred_Pseudo_0,
            pred_Pseudo_1 = pred_Pseudo_1
        ), 0
    )

    list(
        eif_1 = eif_1,
        eif_0 = eif_0
    )
}

transport_ate_incomplete_eif <- function(transport_Npsem, preds, a) {
    `1(S = 1, A = a)` <- transport_Npsem$var("S") & (transport_Npsem$var("A") == a)
    `1(S = 1)` <- transport_Npsem$var("S")
    `1(S = 0)` <- 1 - transport_Npsem$var("S")
    `P(A = a|S = 1, W)` <- a * preds$pred_A + (1 - a) * (1 - preds$pred_A)
    `P(S = 0| V)` <- 1 - preds$pred_S
    `P(S = 1| V)` <- preds$pred_S
    `P(S = 0)` <- mean(1 - transport_Npsem$var("S"))
    `E(Y|S = 1, A, W)` <- preds$pred_Y_a
    `E(Y|S = 1, A = a, W)` <- preds[[paste0("pred_Y_", a)]]
    `E(E(Y|S = 1, A = a, W) | S = 1, V)` <- preds[[paste0("pred_Pseudo_", a)]]
    Y <- transport_Npsem$var("Y")

    (`1(S = 1, A = a)` / `P(A = a|S = 1, W)`) * (`P(S = 0| V)` / (`P(S = 1| V)`) * `P(S = 0)`) *
        (Y - `E(Y|S = 1, A, W)`) +
        (`1(S = 1)` / `P(S = 0)`) * (`P(S = 0| V)` / `P(S = 1| V)`) *
        (`E(Y|S = 1, A = a, W)` - `E(E(Y|S = 1, A = a, W) | S = 1, V)`) +
        (`1(S = 0)` / `P(S = 0)`) * `E(E(Y|S = 1, A = a, W) | S = 1, V)`
}
