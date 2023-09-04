library(tidyverse)
library(rstan)
library(triangle)
rstan_options(auto_write = TRUE)


N <- 18

y <- c(rep(1,12), rep(0,6))
stan_data <- list(N=N, y=y)
stan_data


bin_unif_model <-
'
data {
  int<lower = 0> N;
  int<lower = 0, upper = 1> y[N];
}

parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  theta ~ uniform(0,1); // prior //normal(0.3,0.1)
  y ~ bernoulli(theta); // likelihood
}

'


bin_unif <- stan_model(model_code = bin_unif_model)
set.seed(42)
fit <- sampling(bin_unif, data = stan_data)
print(fit)
stan_dens(fit)
