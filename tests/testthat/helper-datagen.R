gendata_ate <- function(n, A = NULL) {
  W <- rbinom(n, 1, 0.5)
  V <- rbinom(n, 1, 0.66)
  Z <- rbinom(n, 1, 0.33)

  if (is.null(A)) A <- rbinom(n, 1, 0.5)

  S <- rbinom(n, 1, 0.4 + 0.5*W - 0.3*Z)

  Yi <- rnorm(n, A + W + A*V + 2.5*A*Z, sqrt((0.1 + 0.8*W)^2))
  Y <- ifelse(S == 1, Yi, NA_real_)

  data.frame(W = W,
             V = V,
             Z = Z,
             S = S,
             A = A,
             Y = Y,
             Yi = Yi)
}

gendata_ittate <- function(n, A = NULL) {
  S <- rbinom(n, 1, 0.5)
  W1 <- rbinom(n, 1, 0.4 + (0.2 * S))
  W2 <- rnorm(n, 0.1 * S, 1)
  W3 <- rnorm(n, 1 + (0.2 * S), 1)
  if (is.null(A)) A <- rbinom(n, 1, 0.5)
  Z <- rbinom(n, 1, plogis(-log(1.6) + log(4)*A - log(1.1)*W2 - log(1.3)*W3))
  Yi <- rbinom(n, 1, plogis(log(1.6) + log(1.9)*Z - log(1.3)*W3 - log(1.2)*W1 + log(1.2)*W1*Z))
  Y <- ifelse(S == 1, Yi, NA_real_)

  data.frame(W1 = W1, W2 = W2, W3 = W3,
             S = S,
             A = A,
             Z = Z,
             Y = Y,
             Yi = Yi)
}
