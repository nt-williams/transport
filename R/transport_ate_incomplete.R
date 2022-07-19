#' Transport average treatment effects under incomplete effect modification
#'
#' @param transport_Npsem
#' @param learners
#' @param family [\code{character(1)}]\cr
#'  Outcome variable type (i.e., continuous, binomial).
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
transport_ate_incomplete <- function(transport_Npsem, learners, family, ...) {
    # P(S | V)
    fit_S <- regress(transport_Npsem$var("V"), transport_Npsem$var("S"), learners, "binomial", 10)
    pred_S <- regress_predict(fit_S, transport_Npsem$data)

    # P(Z | S, W)
    fit_Z <- regress(transport_Npsem$history("Z"), transport_Npsem$var("Z"), learners, "binomial", 10)
    pred_Z <- regress_predict(fit_Z, transport_Npsem$modify("S", 1))

    # E(Y| S=1, Z, W)
    fit_Y <- regress(transport_Npsem$history("Y")[transport_Npsem$var("S") == 1, ],
                     transport_Npsem$var("Y")[transport_Npsem$var("S") == 1],
                     learners, family, 10)
    pred_Y_z <- regress_predict(fit_Y, transport_Npsem$data)
    pred_Y_1 <- regress_predict(fit_Y, transport_Npsem$modify("Z", 1))
    pred_Y_0 <- regress_predict(fit_Y, transport_Npsem$modify("Z", 0))

    # E(E(Y| S=1, Z=z, W) | S=1, V)
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
            pred_Z = pred_Z,
            pred_Y_z = pred_Y_z,
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
            pred_Z = pred_Z,
            pred_Y_z = pred_Y_z,
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

transport_ate_incomplete_eif <- function(transport_Npsem, preds, z) {
    `1(S = 1, Z = z)` <- as.numeric(transport_Npsem$var("S") & (transport_Npsem$var("Z") == z))
    `1(S = 1)` <- transport_Npsem$var("S")
    `1(S = 0)` <- 1 - transport_Npsem$var("S")
    `P(Z = z|S = 1, W)` <- z * preds$pred_Z + (1 - z) * (1 - preds$pred_Z)
    `P(S = 0| V)` <- 1 - preds$pred_S
    `P(S = 1| V)` <- preds$pred_S
    `P(S = 0)` <- mean(1 - transport_Npsem$var("S"))
    `E(Y|S = 1, Z, W)` <- preds$pred_Y_z
    `E(Y|S = 1, Z = z, W)` <- preds[[paste0("pred_Y_", z)]]
    `E(E(Y|S = 1, Z = z, W) | S = 1, V)` <- preds[[paste0("pred_Pseudo_", z)]]
    Y <- transport_Npsem$var("Y")

    (`1(S = 1, Z = z)` / `P(Z = z|S = 1, W)`) *
        (`P(S = 0| V)` / (`P(S = 1| V)`) * `P(S = 0)`) *
        (Y - `E(Y|S = 1, Z, W)`) +
        (`1(S = 1)` / `P(S = 0)`) * (`P(S = 0| V)` / `P(S = 1| V)`) *
        (`E(Y|S = 1, Z = z, W)` - `E(E(Y|S = 1, Z = z, W) | S = 1, V)`) +
        (`1(S = 0)` / `P(S = 0)`) * `E(E(Y|S = 1, Z = z, W) | S = 1, V)`
}
