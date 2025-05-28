#sample size calculations PIPELINE RSV

library(pwr)

pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, n = (1100*0.9)/2, alternative = "two.sided")

#proportion power calculation for binomial distribution (arcsine transformation) 

#h = 0.1924743
#n = 495
#sig.level = 0.05
#power = 0.8572552
#alternative = two.sided

pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, n = (900*0.9)/2, alternative = "two.sided")

# proportion power calculation for binomial distribution (arcsine transformation) 
# 
# h = 0.1924743
# n = 405
# sig.level = 0.05
# power = 0.7820095
# alternative = two.sided

pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, n = (1400*0.9)/2, alternative = "two.sided")
> pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, n = (1400*0.9)/2, alternative = "two.sided")

# Difference of proportion power calculation for binomial distribution (arcsine transformation) 
# 
# h = 0.1924743
# n = 630
# sig.level = 0.05
# power = 0.9273197
# alternative = two.sided



pwr.2p.test(h = ES.h(p1 = 0.1,p2 = 0.05),sig.level = 0.05, power = 0.80, alternative = "two.sided")

# proportion power calculation for binomial distribution (arcsine transformation) 
# 
# h = 0.1924743
# n = 423.7319
# sig.level = 0.05
# power = 0.8
# alternative = two.sided

2*423.7319*1.1#to allow for 10% loss to follow
#[1] 932.2102

p.out <- pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, power = 0.80, alternative = "two.sided")
plot(p.out)


p.out_90 <- pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, power = 0.90, alternative = "two.sided")

# Difference of proportion power calculation for binomial distribution (arcsine transformation) 
# 
# h = 0.1924743
# n = 567.2579
# sig.level = 0.05
# power = 0.9
# alternative = two.sided
plot(p.out_90)

2*567.2579*1.1#to allow for 10% loss to follow up
#[1] 1247.967
