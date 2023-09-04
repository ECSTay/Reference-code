library(bookdown)
library(officedown)
library(rethinking)
library(truncnorm)
library(rstan)
library(simstudy)
library(dplyr)
library(printr)
library(tidyverse)
library(flextable)
library(bayesplot)
library(pROC)



N = 2000
Asim <-abs(rtruncnorm(N,mean = 43.5, sd = 18.6))

Asim<-as.integer(Asim)
dummy <- function(Asim) {if (Asim < 50) {A<-0} else {A<-1}}

hist(Asim)
print(summary(Asim))

A <- lapply(Asim, dummy)
A <- unlist(A)


reaction <- function(A) {if (A > 0) {S <- rbern(1,0.4)} else {S <- rbern(1, 0.6)}}

S <- lapply(A, reaction)
S <- unlist(S)

dat <- data.frame(A,S)

response <- function(dat) 
{A = dat[1]
S = dat[2]
if( A > 0 & S > 0) {R <- rbern(1, 0.85)}

else if( A > 0 & S < 1 )  {R <- rbern(1, 0.4)} 

else if( A < 1 & S > 0 ) {R <- rbern(1, 0.6)}

else  {R <- rbern(1, 0.15)} #(A < 1 & S < 1)

return(R)

}

R <- apply(dat, 1 ,response)
R <- unlist(R)

dat$R <- R

dat_young <- dat %>% filter(A == 0)
dat_old <- dat %>% filter(A == 1)
###Simulating SMA

seek <- function(dat) 
{A = dat[1]
S = dat[2]
if( A > 0 & S > 0) {SMA <- rbern(1, 0.15)} #0.00595 (P(RMA = 1| A = 1, S = ?) from signal detection data)

else if( A > 0 & S < 1 )  {SMA <- rbern(1, 0.05)} #0.001

else if( A < 1 & S > 0 ) {SMA <- rbern(1, 0.30)}#0.01 (P(RMA = 1| A = 0, S = ?) from signal detection data)

else  {SMA <- rbern(1, 0.005)} #(A < 1 & S < 1) #0.001

return(SMA)

}

SMA <- apply(dat, 1 ,seek)
SMA <- unlist(SMA)

dat$SMA <- SMA

reportMA <- function(dat)
{R = dat[3]
SMA = dat[4]
if (R > 0 & SMA > 0) {RMA <- 1} #RMA <- rbern(1,0.999)

else {RMA <- 0} #RMA <- rbern(1,0.0001)

return(RMA)

}

RMA <- apply(dat, 1, reportMA)
RMA <- unlist(RMA)
dat$RMA <- RMA
  
dat_RMA <- dat %>% filter(R == 1 & SMA == 1)
length(dat_RMA$R)

length(dat$R)

length(dat_RMA$R)/length(dat$R)  


######################################################
#DAG4

RepMA <- "data{
    int N;
    int R[N];
    int S[N];
    int A[N];
    int SMA[N];
    int RMA[N];
}
parameters{
    real ap;
    real bAR;
    real bSR;
    real<lower=0> sigmap;
    real ag;
    real bASMA;
    real bSSMA;
    real<lower=0> sigmag;
    real bRRMA;
    real bSRMA;
    real<lower=0> sigmah;
    real aq;
    real bAS;
    real<lower=0> sigmaq;
}
transformed parameters{
  vector[N] p;
  vector[N] q;
  vector[N] g;
  vector[N] h;
  for(i in 1:N) q[i] = inv_logit(aq + bAS*A[i]);
  for(i in 1:N) p[i] = inv_logit(ap + bAR*A[i] + bSR*S[i]);
  for(i in 1:N) g[i] = inv_logit(ag + bASMA*A[i] + bSSMA*S[i]);
  for(i in 1:N) h[i] = inv_logit(bRRMA*A[i] + bSRMA*S[i]);
}
model{
    
    //priors
    sigmaq ~ exponential( 1 );
    bAS ~ normal(0, 1);
    aq ~ normal(0, sigmaq);
    
    //likelihood
    S ~ bernoulli(q);
    
    //priors
    sigmap ~ exponential(1);
    bSR ~ normal( 0 , 1 );
    bAR ~ normal( 0 , 1 );
    ap ~ normal( 0 , sigmap );
    
    //likelihood
    R ~ bernoulli( p );
    
    //priors
    sigmag ~ exponential(1);
    bSSMA ~ normal( 0 , 1 );
    bASMA ~ normal( 0 , 1 );
    ag ~ normal( 0 , sigmag );
    
    //likelihood
    SMA ~ bernoulli( g );
    
    //priors
   
    bRRMA ~ normal( 0 , 1 );
    bSRMA ~ normal( 0 , 1 );
    
    
    //likelihood
    RMA ~ bernoulli( h );
    
}

generated quantities {
  //generate predictions
  real qA0 = inv_logit(aq);  //pA0 = P(S=1|A=0)
  real qA1 = inv_logit(aq + bAS); //pA1 = P(S=1|A=1)
  real pA0S0 = inv_logit(ap); //P(R=1|A=0,S=0)
  real pA0S1 = inv_logit(ap + bSR); //P(R=1|A=0,S=1)
  real pA1S0 = inv_logit(ap + bAR); //P(R=1|A=1,S=0)
  real pA1S1 = inv_logit(ap + bAR + bSR); //P(R=1|A=1,S=1)
  real gA0S0 = inv_logit(ag); //P(R=1|A=0,S=0)
  real gA0S1 = inv_logit(ag + bSSMA); //P(R=1|A=0,S=1)
  real gA1S0 = inv_logit(ag + bASMA); //P(R=1|A=1,S=0)
  real gA1S1 = inv_logit(ag + bASMA + bSSMA); //P(R=1|A=1,S=1)
  real hR1SMA1 = inv_logit(bRRMA + bSRMA); //P(RMA=1|R=1,SMA=1)
 //prior predictive check
 
  real<lower=0> sigsim = exponential_rng(1);
  real apsim = normal_rng(0,sigsim);
  real bARsim = normal_rng(0,1);
  real bSRsim = normal_rng(0,1);
  real agsim = normal_rng(0,sigsim);
  real bASMAsim = normal_rng(0,1);
  real bSSMAsim = normal_rng(0,1);
  real bARMAsim = normal_rng(0,1);
  real bSRMAsim = normal_rng(0,1);

  vector[N] psim;
  vector[N] Rsim;
  vector[N] gsim;
  vector[N] SMAsim;
  vector[N] hsim;
  vector[N] RMAsim;
  
  for ( i in 1:N ) {
      psim[i] = apsim + bARsim * A[i] + bSRsim * S[i]; 
     psim[i] = inv_logit(psim[i]);
      Rsim[i] = bernoulli_rng(psim[i]);
      
      
    }
   for ( i in 1:N ) {
     
      gsim[i] = agsim + bASMAsim * A[i] + bSSMAsim * S[i]; 
      gsim[i] = inv_logit(gsim[i]);
      SMAsim[i] = bernoulli_rng(gsim[i]);
      
   }
    for ( i in 1:N ) {
     
      hsim[i] = bARMAsim * A[i] + bSRMAsim * S[i];
      hsim[i] = inv_logit(hsim[i]);
      RMAsim[i] = bernoulli_rng(hsim[i]);
      
    }
}


"

options(mc.cores = parallel::detectCores())
dat1 <- list(S = S, R = R, A = A, RMA = RMA, SMA = SMA, N = N)

modPPC <- stan(model_code = RepMA, data = dat1,
                iter = 5000, warmup=100)

params <- summary(modPPC, pars = c("ap", "bAR", "bSR", "aq", "bAS","bRRMA","bSRMA",
                                     "qA0", "qA1","pA0S0","pA0S1","pA1S1","pA1S0", 
                                   "gA0S0","gA0S1","gA1S1","gA1S0","hR1SMA1"))$summary
print(params)


bb <- as.matrix(modPPC, pars = c("Rsim"))
Rsim <- apply(bb,2,mean)

RRsim = Rsim
RRsim[Rsim <0.5] = 0
RRsim[Rsim >= 0.5] = 1

table(RRsim,dat1$R)
#plot(dat1$R, RRsim)


library(pROC)
roc(dat1$R ~ Rsim, plot = TRUE, print.auc = TRUE)

dd <- as.matrix(mod1_mod, pars = c("RMAsim"))
RMAsim <- apply(dd,2,mean)

roc(dat1$RMA ~ RMAsim, plot = TRUE, print.auc = TRUE)

RMASsim = RMAsim
RMASsim[RMAsim <0.5] = 0
RMASsim[RMAsim >= 0.5] = 1

table(RMASsim,dat1$RMA)







###########DAG3

SMA <- "data{
  int N;
  int R[N];
  int S[N];
  int A[N];
  int SMA[N];
}
parameters{
  real ap;
  real bAR;
  real bSR;
  real<lower=0> sigmap;
  real ag;
  real bASMA;
  real bSSMA;
  real<lower=0> sigmag;
  real aq;
  real bAS;
  real<lower=0> sigmaq;
}
transformed parameters{
  vector[N] p;
  vector[N] q;
  vector[N] g;
  for(i in 1:N) q[i] = inv_logit(aq + bAS*A[i]);
  for(i in 1:N) p[i] = inv_logit(ap + bAR*A[i] + bSR*S[i]);
  for(i in 1:N) g[i] = inv_logit(ag + bASMA*A[i] + bSSMA*S[i]);
}
model{
  
  //priors
  sigmaq ~ exponential( 1 );
  bAS ~ normal(0, 1);
  aq ~ normal(0, sigmaq);
  
  //likelihood
  S ~ bernoulli(q);
  
  //priors
  sigmap ~ exponential(1);
  bSR ~ normal( 0 , 1 );
  bAR ~ normal( 0 , 1 );
  ap ~ normal( 0 , sigmap );
  
  //likelihood
  R ~ bernoulli( p );
  
  //priors
  sigmag ~ exponential(1);
  bSSMA ~ normal( 0 , 1 );
  bASMA ~ normal( 0 , 1 );
  ag ~ normal( 0 , sigmag );
  
  //likelihood
  SMA ~ bernoulli( g );
  
}

generated quantities {
  //generate predictions
  real qA0 = inv_logit(aq);  //pA0 = P(S=1|A=0)
  real qA1 = inv_logit(aq + bAS); //pA1 = P(S=1|A=1)
  real pA0S0 = inv_logit(ap); //P(R=1|A=0,S=0)
  real pA0S1 = inv_logit(ap + bSR); //P(R=1|A=0,S=1)
  real pA1S0 = inv_logit(ap + bAR); //P(R=1|A=1,S=0)
  real pA1S1 = inv_logit(ap + bAR + bSR); //P(R=1|A=1,S=1)
  real gA0S0 = inv_logit(ag); //P(R=1|A=0,S=0)
  real gA0S1 = inv_logit(ag + bSSMA); //P(R=1|A=0,S=1)
  real gA1S0 = inv_logit(ag + bASMA); //P(R=1|A=1,S=0)
  real gA1S1 = inv_logit(ag + bASMA + bSSMA); //P(R=1|A=1,S=1)
  //prior predictive check
  
  real<lower=0> sigsim = exponential_rng(1);
  real apsim = normal_rng(0,sigsim);
  real bARsim = normal_rng(0,1);
  real bSRsim = normal_rng(0,1);
  real agsim = normal_rng(0,sigsim);
  real bASMAsim = normal_rng(0,1);
  real bSSMAsim = normal_rng(0,1);
  
  vector[N] psim;
  vector[N] Rsim;
  vector[N] gsim;
  vector[N] SMAsim;
  
  for ( i in 1:N ) {
    psim[i] = apsim + bARsim * A[i] + bSRsim * S[i]; #A is from the data
    psim[i] = inv_logit(psim[i]);
    Rsim[i] = bernoulli_rng(psim[i]);
    
    
  }
  for ( i in 1:N ) {
    
    gsim[i] = agsim + bASMAsim * A[i] + bSSMAsim * S[i]; #A is from the data
    gsim[i] = inv_logit(gsim[i]);
    SMAsim[i] = bernoulli_rng(gsim[i]);
    
  }
}"

options(mc.cores = parallel::detectCores())
dat3 <- list(S = S, R = R, A = A, N = N, SMA = SMA)

mod3 <- stan(model_code = SMA, data = dat3,
               iter = 2000, warmup = 100 )

params <- summary(mod3, pars = c("ap", "bAR", "bSR", "aq", "bAS", "ag", "ah","bASMA","bSSMA",
                                   "qA0", "qA1","pA0S0","pA0S1","pA1S1","pA1S0",
                                   "gA0S0","gA0S1","gA1S0", "gA0S0"))$summary
print(params)


bb <- as.matrix(mod3, pars = c("Rsim"))
Rsim <- apply(bb,2,mean)

RRsim = Rsim
RRsim[Rsim <0.5] = 0
RRsim[Rsim >= 0.5] = 1

table(RRsim,dat3$R)
#plot(dat1$R, RRsim)
library(pROC)
roc(dat3$R ~ Rsim, plot = TRUE, print.auc = TRUE)

cc <- as.matrix(mod3, pars = c("SMAsim"))
SMAsim <- apply(cc,2,mean)

roc(dat3$SMA ~ SMAsim, plot = TRUE, print.auc = TRUE)

SMASsim = SMAsim
SMASsim[SMAsim <0.5] = 0
SMASsim[SMAsim >= 0.5] = 1

table(SMASsim,dat3$SMA)


