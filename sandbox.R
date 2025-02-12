simulate_data <- function(n) {
  # Generate the data
  S <- rbinom(n, 1, 0.5)
  W1 <- rbinom(n, 1, 0.4 + 0.2 * S)
  W2 <- rnorm(n, 0.1 * S, 1)
  W3 <- rnorm(n, 1 + 0.2 * S, 1)
  A <- rbinom(n, 1, 0.5)

  # Calculate the probability for Z
  logit_Z <- -log(1.6) + log(4) * A - log(1.1) * W2 - log(1.3) * W3
  prob_Z <- exp(logit_Z) / (1 + exp(logit_Z))
  Z <- rbinom(n, 1, prob_Z)

  # Calculate the probability for Y
  logit_Y <- log(1.6) + log(1.9) * Z - log(1.3) * W3 - log(1.2) * W1 + log(1.2) * A * W1
  prob_Y <- exp(logit_Y) / (1 + exp(logit_Y))
  Y <- rbinom(n, 1, prob_Y)

  # Combine the generated data into a data frame
  simulated_data <- data.frame(S, W1, W2, W3, A, Z, Y)

  return(simulated_data)
}

# Example usage
n <- 1000
simulated_data <- simulate_data(n)

transport_ittate(simulated_data,
                 "A", "Z", "Y", c("W1", "W2", "W3"), "S",
                 learners_trt = "glm",
                 learners_pop = "glm",
                 learners_outcome = "glm",
                 learners_ptc = "glm",
                 folds = 1,
                 control = transport_control())





library(data.table)

setDT(simulated_data)

tmp1 <- simulated_data[, .(S, W1, W2, W3, A, Z)]
tmp2 <- copy(tmp1)

tmp1[, let(delta = 1)]
tmp2[, let(delta = 0)]
tmp2[, let(Z = sample(unique(Z), .N, replace = TRUE))]
tmp <- rbind(tmp1, tmp2)

fit <- glm(delta ~ ., data = tmp, family = "binomial")

u1 <- predict(fit, tmp1[, let(S = 0)], type = "response")
u2 <- predict(fit, tmp1[, let(S = 1)], type = "response")

(u1 / (1 - u1)) * ((1 - u2) / u2)


