#' Title
#'
#' @param transport_Npsem
#' @param learners
#' @param family
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
transport_ate <- function(transport_Npsem, learners, family, ...) {
    # P(S | W)
    fit_S <- regress(transport_Npsem$var("W"), transport_Npsem$var("S"), learners, "binomial", 10)
    pred_S <- regress_predict(fit_S, transport_Npsem$var("W"))

    # P(Z | S, W)
    fit_Z <- regress(transport_Npsem$history("Z"), transport_Npsem$var("Z"), learners, "binomial", 10)
    pred_Z <- regress_predict(fit_Z, transport_Npsem$modify("S", 1)$history("Z"))

    # E(Y| S=1, Z, W)
    fit_Y <- regress(transport_Npsem$history("Y")[transport_Npsem$var("S") == 1, ],
                     transport_Npsem$var("Y")[transport_Npsem$var("S") == 1],
                     learners, family, 10)
    pred_Y_z <- regress_predict(fit_Y, transport_Npsem$history("Y"))
    pred_Y_1 <- regress_predict(fit_Y, transport_Npsem$modify("Z", 1)$history("Y"))
    pred_Y_0 <- regress_predict(fit_Y, transport_Npsem$modify("Z", 0)$history("Y"))

    # Influence function
    z <- transport_Npsem$var("Z")
    s <- transport_Npsem$var("S")
    pred_Z_z <- z * pred_Z + (1 - z) * (1 - pred_Z)
    ps0 <- mean(1 - transport_Npsem$var("S"))
    y <- transport_Npsem$var("Y")

    (((2*z - 1) * s) / pred_Z_z) * ((1 - pred_S) / (pred_S * ps0)) * (y - pred_Y_z) +
        ((1 - s) / ps0) * (pred_Y_1 - pred_Y_0)
}
