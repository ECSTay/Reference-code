#invlogit to determine hyperparameter for alpha

0.7 = exp(a)/(1+exp(a))

plogis(0.6) #0.646

plogis(0.7) #0.668
 plogis(0.15) #0.54
#OR
p <- 0.7#P(R=1|S=1)
q <- 1-p

log((p/(1-p))*((1-q)/q))

#OR = odds of "what we want"/odds of what "we don't want'

#Beta_s = log odds ratio of someone responding with a severe reaction


x <- rnorm(10000,0,2)
x2 <- invlogit(x)
plot(density(x2))
