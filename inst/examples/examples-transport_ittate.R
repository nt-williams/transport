gendata <- function(n, A = NULL, S = NULL) {
  if (is.null(S)) S <- rbinom(n, 1, 0.5)
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

truth <- mean(gendata(1e7, 1, 0)$Yi) - mean(gendata(1e7, 0, 0)$Yi)

set.seed(123)
n <- 1000

tmp <- gendata(n)

if (requireNamespace("ranger", quietly = TRUE)) {
  transport_ittate(data = tmp,
                   trt = "Z",
                   instrument = "A",
                   outcome = "Y",
                   covar = c("W1", "W2", "W3"),
                   pop = "S",
                   folds = 1)
}
