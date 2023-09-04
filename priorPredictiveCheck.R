
###########################################################################
## Prior predictive check (PPC)
###########################################################################

## What is this? 
## To verify the prior beliefs about the parameters are reasonable

## How to check this?
## (1) Simulate data from the prior distributions of the model parameters
## (2) Compare it with actual data (in our case data simulated from model parameters)
## (3) Compare their statistical properties: distribution of parameter or data

## How do you develop code and run?

# Actual data: we do not have this so we generate data
n <- 50 # sample size
x <- rnorm(n,5,1) # covariate
e <- rnorm(n) # normal error process
y <- 1 + 0.5 * x + e

# Simulate M sets of data from the prior distributions
M <- 100
alpha <- rnorm(M,0,1)
beta <- rnorm(M,0,1)
yrep <- matrix(NA,M,n)
for(i in 1:M){
  yrep[i,] <- y <- alpha[i] + beta[i] * x + e
}
library(bayesplot)
pp_check(y, yrep[1:M,], ppc_dens_overlay)

###########################################################################
###########################################################################




