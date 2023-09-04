## Effect size calculation 
vaxa <- 0.00000001    # control
vaxb <- 0.0005        # vaccine

effect_size <- (2*asin(sqrt(vaxa))) - (2*asin(sqrt(vaxb)))

## Power calculation for n=10000 per vaccine arm and significance level=5% 

alpha <- 0.05
n_arm <- 2000

pwr.2p.test(h=effect_size, n=n_arm, sig.level=alpha, alternative="less")       # one-side test (wP less)
pwr.2p.test(h=effect_size, n=n_arm, sig.level=alpha, alternative="two.sided")  # two sided test


## Effect size calculation 
vaxa <- 1*10^(-2)    # control
vaxb <- 1.1*10^(-2)        # vaccine

effect_size <- (2*asin(sqrt(vaxa))) - (2*asin(sqrt(vaxb)))

## Power calculation for n=10000 per vaccine arm and significance level=5% 

alpha <- 0.05
n_arm <- 20000

pwr.2p.test(h=effect_size, n=n_arm, sig.level=alpha, alternative="less")       # one-side test (wP less)
pwr.2p.test(h=effect_size, n=n_arm, sig.level=alpha, alternative="two.sided")  # two sided test



#Power calculations for situation A

vaxa <-  0
vaxb <- 

pwr.p.test(h=effect_size, n=n_arm, sig.level=alpha, alternative="less")   