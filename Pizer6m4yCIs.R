#Pfizer6m-4y UL 95%Cis
#The variance of the sample proportion is p(1-p)/n, therefore the standard error is sqrt(p(1-p)/n). 

0.06 + (qnorm(0.975) * sqrt((0.06*(1-0.06))/100)) #10.7
0.06 + (qnorm(0.995) * sqrt((0.06*(1-0.06))/100)) #12.1

#The 95% CI for binomial proportions can also be calculated using prop.test and binomial.test, which use slightly different methods.

prop.test(x=(0.06*100), n=100, conf.level=0.95, correct = FALSE) #12.5
binom.test(x=floor(0.06*100), n=100, conf.level=0.95) #12.6

library(Hmisc)
binconf(x=100*0.06, n=100, alpha=0.05, method="all")

#            PointEst      Lower     Upper
# Exact          0.06 0.02233489 0.1260299
# Wilson         0.06 0.02778612 0.1247682
# Asymptotic     0.06 0.01345343 0.1065466