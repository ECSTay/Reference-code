sgp <- 1 #vax brand and dose that you're interested in
nrecords <- vector(length = (ncol(cusum_data) - 1)/2)
signals <- vector(length = (ncol(cusum_data) - 1)/2)
records <- unlist(cusum_data[,.SD, .SDcols = sgp + 1])
nrecords[sgp] <- sum(records)
events <- unlist(cusum_data[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data) - 1)/2)])


cusum <- function(y, n, p_E, p_M, y_0, C){
  X <- vector(length = length(y) + 1)
  X[1] <- y_0
  signal <- rep(FALSE, length(y))
  OR <- p_M/(1-p_M)*(1-p_E)/p_E
  W <- y*log(OR) - n*log(1-p_E+OR*p_E)
  for(t in 1:length(y)){
    X[t+1] <- max(0, X[t] + W[t])
    if(X[t+1] >= C) signal[t] <- TRUE
  }
  return(list(X = X, signal = signal))
}


cusum(y = events, n = records, p_E = cusum_pars$expected[sgp], p_M = cusum_pars$maximum[sgp], y_0 = 0, C = cusum_pars$control[sgp])#flu

y <- cusum(y = events, n = records, p_E = p_E[sgp], p_M = p_M[sgp], y_0 = y_0[sgp], C = 3)$X#COVID


dat <- data.table(y=y)
x <- vector(length = 14)
class(x) <- "Date"
for(i in 1:length(x)) x[i] = data_date - 7*i
x <- rev(x)
x <- x+7
dat$x <- x
dat

datebreaks <- seq(as.Date("2022-04-11"), as.Date("2202-07-11"), by = "week")
ggplot(dat, aes(x = x, y = y)) + geom_line() + geom_hline(aes(yintercept = 3), col = "red") + ylab("CUSUM") + xlab ("Date") + scale_x_date(breaks=datebreaks) + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ggtitle("MA CUSUM for FluQuadri (Pregnant Women)")
ggplot(dat, aes(x = x, y = y)) + geom_line() + geom_hline(aes(yintercept = 3), col = "red") + ylab("CUSUM") + xlab ("Date")  + ggtitle("MA CUSUM for Fluarix Tetra (Pregnant Women)")
####for COVID19

p_E <- c(pf1kids = 0.008, pf2kids = 0.021, pf3kids = 0.021,
         #pf1kidsxbb = 0.008, pf2kidsxbb = 0.021, pf3kidsxbb = 0.021, 
         pfbiv3under= 0.021, pfbiv3over = 0.013, pfbiv453under= 0.021, pfbiv453over = 0.013, # biv is BA1, biv45 is BA4-5
         spbiv453under= 0.021, spbiv453over = 0.013, #  biv45 is BA4-5
         pfxbb3under = 0.021, pfxbb3over = 0.013, 
         spxbb3under = 0.021, spxbb3over = 0.013) 
# nv1under = 0.030, nv1over = 0.030, nv2under = 0.030, nv2over = 0.030, nv3under = 0.030, nv3over = 0.030)
p_M <- ifelse(str_sub(names(p_E), 1, 2) == "nv", 0.042,
              ifelse(str_detect(names(p_E), "yngkidsfv"), 0.15, 
                     ifelse(str_detect(names(p_E), "yngkidspffv"), 0.126, p_E*3))) #
y_0 <- c(pf1kids = 0, pf2kids = 0, pf3kids = 1.5, #pf1kidsxbb = 0, pf2kidsxbb = 0, pf3kidsxbb = 15,
         pfbiv3under = 1.5, pfbiv3over = 1.5, pfbiv453under = 1.5, pfbiv453over = 1.5,
         spbiv453under = 1.5, spbiv453over = 1.5, pfxbb3under = 1.5, pfxbb3over = 1.5, spxbb3under = 1.5, spxbb3over = 1.5)
sgp <- 10

records <- unlist(cusum_data_sub[,.SD, .SDcols = sgp + 1])
nrecords[sgp] <- sum(records)
events <- unlist(cusum_data_sub[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data_sub) - 1)/2)])
nanalysed[sgp] <- sum(records)
dates[sgp] <- as.character(format(
  as.Date(cusum_data_sub$survey_response_timestamp[min(which(sum(records) == cumsum(cusum_data_sub[,sgp+1,with=F])))]), 
  "%d %B %Y"))