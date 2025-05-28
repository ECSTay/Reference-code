###CUSUM charts for RSV

library(tidyverse)
library(data.table)
library(officer)
library(matrixStats)
library(stringr)
library(ggplot2)
library(here)
library(grid)
library(lubridate)

#data_date <- as.Date("2024-09-30")
data_date <- as.Date("2025-04-30")

cusum_data <- fread(here(paste0("RSV/Data/Signal Detection/cusum_data_", data_date, ".Rds")))

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

n_subgroups <- 3
nrecords <- vector(length = n_subgroups)
nevents <- vector(length = n_subgroups)
signals <- vector(length = n_subgroups)
p_E <- c("Arexvy.60+" = 0.004, "Abrysvo.Pregnant" = 0.016, "Abrysvo.60+" = 0.005)
p_M <- c("Arexvy.60+" = 0.012, "Abrysvo.Pregnant" = 0.048, "Abrysvo.60+" = 0.015)
sgp <-3



records <- unlist(cusum_data[,.SD, .SDcols = sgp + 1])
events <- unlist(cusum_data[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data) - 1)/2)])
nrecords[sgp] <- sum(records)
nevents[sgp] <- sum(events)

events
records

signals[sgp] <- cusum(y = events, n = records, p_E = p_E[sgp], p_M = p_M[sgp], y_0 = 1.5, C = 3)$signal[length(records)]


y <- cusum(y = events, n = records, p_E = p_E[sgp],p_M = p_M[sgp], y_0 = 1.5, C = 3)$X
y <- y[-1]
dat <- data.table(y = y)
x <- cusum_data$survey_response_timestamp
dat$x <- x

ggplot(dat, aes(x = x, y = y, group = 1)) + geom_line() + 
  geom_hline(aes(yintercept = 3), col = "red") + 
  ylab("CUSUM") + xlab ("Date") + 
  #scale_x_date(breaks=datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle("MA CUSUM for Abrysvo >60y 2025-04-30")


