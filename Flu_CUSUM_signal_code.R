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

data_date <- as.Date("2025-05-26")

cusum_data <- fread(here(paste0("Influenza/Data/Signal Detection/2025/cusum_data_", data_date, ".csv")))



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

cusum_pars <- list(expected = c(vaxigrip_6m_5y_fv = 0.102, fluquadri_6m_5y_fv = 0.081, 
                                influvac_6m_5y_fv = 0.042, flucelvax_6m_5y_fv = 0.068, 
                                vaxigrip_6m_5y_ma = 0.010, fluquadri_6m_5y_ma = 0.006,
                                influvac_6m_5y_ma = 0.007, flucelvax_6m_5y_ma = 0.007,
                                vaxigrip_more_5y_ma = 0.004, afluaria_more_5y_ma = 0.0028, 
                                fluquadri_more_5y_ma = 0.0034, influvac_more_5y_ma = 0.0042, 
                                flucelvax_more_5y_ma = 0.0038,
                                fluad_more_60y_ma = 0.0019, fluzone_more_60y_ma = 0.003,
                                vaxigrip_pregnant_ma = 0.0055, afluaria_pregnant_ma = 0.008, 
                                fluquadri_pregnant_ma = 0.004, influvac_pregnant_ma = 0.008, 
                                flucelvax_pregnant_ma = 0.008),
                   maximum = c(vaxigrip_6m_5y_fv = 0.204, fluquadri_6m_5y_fv = 0.162, 
                               influvac_6m_5y_fv = 0.084, flucelvax_6m_5y_fv = 0.136, 
                               vaxigrip_6m_5y_ma = 0.03,fluquadri_6m_5y_ma = 0.018,
                               influvac_6m_5y_ma = 0.021, flucelvax_6m_5y_ma = 0.021,
                               vaxigrip_more_5y_ma = 0.012, afluaria_more_5y_ma = 0.0084, 
                               fluquadri_more_5y_ma = 0.0102, influvac_more_5y_ma = 0.0126, 
                               flucelvax_more_5y_ma = 0.0114,
                               fluad_more_60y_ma = 0.0057, fluzone_more_60y_ma = 0.009,
                               vaxigrip_pregnant_ma = 0.0165, afluaria_pregnant_ma = 0.024, 
                               fluquadri_pregnant_ma = 0.012, influvac_pregnant_ma = 0.024, 
                               flucelvax_pregnant_ma = 0.024),
                   control = c(vaxigrip_6m_5y_fv = 8, fluquadri_6m_5y_fv = 8, 
                               influvac_6m_5y_fv = 8,flucelvax_6m_5y_fv = 8, 
                               vaxigrip_6m_5y_ma = 8, fluquadri_6m_5y_ma = 10,
                               influvac_6m_5y_ma = 8, flucelvax_6m_5y_ma = 8,
                               vaxigrip_more_5y_ma = 8, afluaria_more_5y_ma = 11,
                               fluquadri_more_5y_ma = 7, influvac_more_5y_ma = 5, 
                               flucelvax_more_5y_ma = 11,
                               fluad_more_60y_ma = 9, fluzone_more_60y_ma = 9,
                               vaxigrip_pregnant_ma = 3, afluaria_pregnant_ma = 6, 
                               fluquadri_pregnant_ma = 3, influvac_pregnant_ma = 6, 
                               flucelvax_pregnant_ma = 6))


nrecords_flu <- vector(length = (ncol(cusum_data) - 1)/2)
signals_flu <- vector(length = (ncol(cusum_data) - 1)/2)
sgp <- 20
records_flu <- unlist(cusum_data[,.SD, .SDcols = sgp + 1])
nrecords_flu[sgp] <- sum(records_flu)
events_flu <- unlist(cusum_data[,.SD, .SDcols = sgp + 1 + ((ncol(cusum_data) - 1)/2)])
records_flu
events_flu
                 
y <- cusum(y = events_flu, n = records_flu, p_E = cusum_pars$expected[sgp], p_M = cusum_pars$maximum[sgp], 
           y_0 = cusum_pars$control[sgp]/2, C = cusum_pars$control[sgp])$X


y <- y[-1]
dat <- data.table(y=y)

x <- cusum_data$survey_response_timestamp
dat$x <- x
dat

datebreaks <- seq(as.Date("2025-03-24"), as.Date("2025-05-26"), by = "week")
ggplot(dat, aes(x = x, y = y)) + geom_line() + 
  geom_hline(aes(yintercept = 6), col = "red") + 
  ylim(0,10) +
  ylab("CUSUM") + xlab ("Date") + 
  scale_x_date(breaks=datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle("CUSUM for Flucelvax Quad MA Pregnant")


#ggplot(dat, aes(x = x, y = y)) + geom_line() + geom_hline(aes(yintercept = 3), col = "red") + ylab("CUSUM") + xlab ("Date")  + ggtitle("MA CUSUM for FluQuadri (Pregnant Women)")



