n <- 261
table(dat$rop)
dat_control <- dat[data$study_gp == "1",]
dat_int <- dat[dat$study_gp =="2",]
table(dat_control$rop)
##control = 7, int = 12
##control_no  = 125, int_no  = 117
matrix(7,12,125,117)
stuff <- matrix(c(12,125,17,112), ncol = 2)

prop.test(c(132*0.0606, 129*0.0465), c(132, 129), conf.level = 0.95)
