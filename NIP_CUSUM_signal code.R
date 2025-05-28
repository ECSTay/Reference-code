library(readxl)
library(here)
library(data.table)
library(tidyverse)
library(stringr)
library(officer)
library(flextable)
library(cmdstanr)
library(matrixStats)
library(forcats)
library(ozmaps)
library(scales)
  
data_date <- as.Date("2025-03-31")


cusum_data_NIP <- fread(here::here("All NIP/Data/Clean/cusum_data.csv"))

##NIP code
cusum <- function(y, n, p_E, p_M, y_0, C){
  X <- vector(length = length(y) + 1)
  X[1] <- y_0
  signal <- rep(FALSE, length(y))
  OR <- p_M/(1-p_M) * (1-p_E)/p_E
  W <- y*log(OR) - n*log(1-p_E+OR*p_E)
  for(t in 1:length(y)){
    X[t+1] <- max(0, X[t] + W[t])
    if(X[t+1] >= C) signal[t] <- TRUE
  }
  return(list(X = X, signal = signal))
}


schedules <- c("2 months" ,"4 months" , "6 months" ,"12 months" , "18 months" , "4 years" , "Adolescent HPV & dTpa",
               "Adolescent MenACWY","Pregnant dTpa" , "Older adult 13vPCV", "Older adult zoster")


p_E <- c(0.008, 0.009, 0.007, 0.01, 0.014, 0.019, 0.007,  0.005,  0.004, 0.005,  0.005)
C <- c( 8, 7, 10, 6, 10, 14, 7,  11, 10, 7, 4.5)

sgp <- 2 #4 months

nrecords <- vector(length = ncol(cusum_data_NIP))
events <- unlist(cusum_data_NIP[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data_NIP) - 1)/2)])
signals <- vector(length = ncol(cusum_data_NIP))
records <- unlist(cusum_data_NIP[,.SD, .SDcols = sgp + 1])
nrecords[sgp] <- sum(records)
records
events

records/events

y <- cusum(y = events, n = records, p_E = p_E[sgp], p_M = p_E[sgp]*3, y_0 = C[sgp]/2, C = C[sgp])$X


y <- y[-1]
dat <- data.table(y=y)

x <- cusum_data_NIP$Date
dat$x <- x
dat

datebreaks <- as.Date("2022-06-30", "2022-09-30", "2022-12-31", "2023-03-31", "2023-07-07", "2023-10-10", "2024-01-15", "2024-03-31", "2024-06-30",
                      "2024-09-30", "2024-12-31", "2025-03-31")


ggplot(dat, aes(x = x, y = y)) + geom_line() + 
  geom_hline(aes(yintercept = 7), col = "red") + 
  ylab("CUSUM") + xlab ("Date") + 
  scale_x_date(breaks=datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  #labs(x = datebreaks) +
  ggtitle("MA CUSUM for NIP 4 months Schedule")
