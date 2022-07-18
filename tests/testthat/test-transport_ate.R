sim_data <- function(n) {
    S <- rbinom(n, 1, 0.5)
    W1 <- rbinom(n, 1, 0.4 + (0.2 * S))
    W2 <- rnorm(n, 0.1*S, 1)
    W3 <- rnorm(n, 1 + (0.2*S), 1)
    A <- rbinom(n, 1, 0.5)
    Z0 <- rbinom(n , 1, plogis( -log(1.6) - log(1.1)*W2 -log(1.3)*W3))
    Z1 <- rbinom(n, 1, plogis(  -log(1.6) +log(4) - log(1.1)*W2  -log(1.3)*W3))
    Z <- ifelse(A == 1, Z1, Z0)
    Y0 <- rbinom(n, 1, plogis(log(1.6)  + (log(1.9)*Z0) -log(1.3)*W3 - log(1.2)*W1 + log(1.2)*W1*Z0))
    Y1 <- rbinom(n, 1, plogis(log(1.6)  + (log(1.9)*Z1) -log(1.3)*W3 - log(1.2)*W1 + log(1.2)*W1*Z1))
    Y <- ifelse(A == 1, Y1, Y0)
    list(obs = data.frame(W1 = W1, W2 = W2, W3 = W3, S = S, A = A, Z = Z, Y = Y, Y1 = Y1, Y0 = Y0),
         ate = mean((Y1 - Y0)))
}

dat <- sim_data(1000)

Np <- transport_Npsem$new(dat$obs, c("W1", "W2", "W3"), V = c("W1", "W3"), A = "A", Z = NULL, S = "S", Y = "Y")
learners <- list(
    S = "SL.glm", Z = "SL.glm", Y = "SL.glm"
)

test <- transport_ate(Np, c("SL.glm", "SL.earth", "SL.ranger"), "binomial")
mean(test$eif_1 - test$eif_0)
