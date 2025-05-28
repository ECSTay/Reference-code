###CUSUM charts for COVID-19

library(tidyverse)
library(data.table)
library(officer)
library(matrixStats)
library(stringr)
library(ggplot2)
library(here)
library(grid)
library(lubridate)

data_date <- as.Date("2025-03-17")

cusum_data <- readRDS(here(paste0("R files/Cleaning and analysis/Signal detection/Data/cusum_data_", data_date, ".Rda")))

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

nrecords <- vector(length = (ncol(cusum_data) - 1)/2)
nanalysed <- vector(length = (ncol(cusum_data) - 1)/2)
dates <- vector(length = (ncol(cusum_data) - 1)/2)
signals <- vector(length = (ncol(cusum_data) - 1)/2)
p_E <- c(pf1kids = 0.008, pf2kids = 0.021, pf3kids = 0.021,
         #pf1kidsxbb = 0.008, pf2kidsxbb = 0.021, pf3kidsxbb = 0.021, 
         pfbiv453under= 0.021, pfbiv453over = 0.013, # biv45 is BA4-5
         pfxbb3under = 0.021, pfxbb3over = 0.013,
         pfjunder = 0.021, pfjnover = 0.013)
p_M <- ifelse(str_sub(names(p_E), 1, 2) == "nv", 0.042,
              ifelse(str_detect(names(p_E), "yngkidsfv"), 0.15, 
                     ifelse(str_detect(names(p_E), "yngkidspffv"), 0.126, p_E*3))) #
y_0 <- c(pf1kids = 0, pf2kids = 0, pf3kids = 1.5, #pf1kidsxbb = 0, pf2kidsxbb = 0, pf3kidsxbb = 15,
         pfbiv453under = 1.5, pfbiv453over = 1.5,
         pfxbb3under = 1.5, pfxbb3over = 1.5,
         pfjunder = 1.5, pfjnover = 1.5)
#pf1kids sgp 1 2022-01-10 cusum_data
#pf2kids sgp 2 2022-02-07 
cusum_data[-c(1,2),]
#pf3kids sgp 3 2022-04-04 6
cusum_data <- cusum_data[-c(1,2),]#check

#BA4-5 sgp 4 and 5 2023-03-06 
cusum_data <- cusum_data[-c(1:24),]
#XBB sgp 6 and 7 2023_12_25
cusum_data <- cusum_data[-c(1:18),]
#JN1 sgp 8 and 9 2024_12_23
cusum_data <- cusum_data[-c(1:13),]

sgp <- 9 #check rows of cusum_data correspond with group

  records <- unlist(cusum_data[,.SD, .SDcols = sgp + 1])
  nrecords[sgp] <- sum(records)
  events <- unlist(cusum_data[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data) - 1)/2)])
  nanalysed[sgp] <- sum(records)
  #signals[sgp] <- cusum(y = events, n = records, p_E = p_E[sgp],
                      #  p_M = p_M[sgp], y_0 = y_0[sgp], C = 3)$signal[length(records)]



y <- cusum(y = events, n = records, p_E = p_E[sgp],p_M = p_M[sgp], y_0 = y_0[sgp], C = 3)$X
y <- y[-1]
dat <- data.table(y = y)
x <- cusum_data$survey_response_timestamp
dat$x <- x

datebreaks <- seq(as.Date("2024-12-23"), as.Date("2024-03-17"), by = "week")
#For JN.1
datebreaks <- c(as.Date("2024-12-23"), as.Date("2024-01-20"), as.Date("2024-03-17"), as.Date("2024-03-17"))
ggplot(dat, aes(x = x, y = y, group = 1)) + geom_line() + 
  geom_hline(aes(yintercept = 1.5), col = "red") + 
  ylab("CUSUM") + xlab ("Date") + 
  #scale_x_date(breaks=datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle("MA CUSUM for Pfizer JN1 >50")


