library(data.table)
library(here)
library(ggplot2)
data_date <- as.Date("2023-08-28")
cusum_data <- fread(here(paste0("Influenza/Data/Signal Detection/cusum_data_", data_date, ".csv")))

sgp <- 2
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
  
cusum_pars <- list(expected = c(vaxigrip_6m_5y_fv = 0.0725, fluarix_6m_5y_fv = 0.06,
                                fluquadri_6m_5y_fv = 0.032, influvac_6m_5y_fv = 0.042,
                                flucelvax_6m_5y_fv = 0.042, vaxigrip_6m_5y_ma = 0.009,
                                fluarix_6m_5y_ma = 0.008, fluquadri_6m_5y_ma = 0.006,
                                influvac_6m_5y_ma = 0.007, flucelvax_6m_5y_ma = 0.007,
                                vaxigrip_more_5y_ma = 0.004, fluarix_more_5y_ma = 0.0035,
                                afluaria_more_5y_ma = 0.004, fluquadri_more_5y_ma = 0.0035,
                                influvac_more_5y_ma = 0.005, flucelvax_more_5y_ma = 0.004,
                                fluad_more_60y_ma = 0.0025, fluzone_more_60y_ma = 0.003,
                                vaxigrip_pregnant_ma = 0.006, fluarix_pregnant_ma = 0.006,
                                afluaria_pregnant_ma = 0.008, fluquadri_pregnant_ma = 0.003,
                                influvac_pregnant_ma = 0.008, flucelvax_pregnant_ma = 0.008),
                   maximum = c(vaxigrip_6m_5y_fv = 0.145, fluarix_6m_5y_fv = 0.12,
                               fluquadri_6m_5y_fv = 0.064, influvac_6m_5y_fv = 0.084,
                               flucelvax_6m_5y_fv = 0.084, vaxigrip_6m_5y_ma = 0.027,
                               fluarix_6m_5y_ma = 0.024, fluquadri_6m_5y_ma = 0.018,
                               influvac_6m_5y_ma = 0.021, flucelvax_6m_5y_ma = 0.021,
                               vaxigrip_more_5y_ma = 0.012, fluarix_more_5y_ma = 0.0105,
                               afluaria_more_5y_ma = 0.012, fluquadri_more_5y_ma = 0.0105,
                               influvac_more_5y_ma = 0.015, flucelvax_more_5y_ma = 0.012,
                               fluad_more_60y_ma = 0.0075, fluzone_more_60y_ma = 0.009,
                               vaxigrip_pregnant_ma = 0.018, fluarix_pregnant_ma = 0.018,
                               afluaria_pregnant_ma = 0.024, fluquadri_pregnant_ma = 0.009,
                               influvac_pregnant_ma = 0.024, flucelvax_pregnant_ma = 0.024),
                   control = c(vaxigrip_6m_5y_fv = 8, fluarix_6m_5y_fv = 8,
                               fluquadri_6m_5y_fv = 8, influvac_6m_5y_fv = 8,
                               flucelvax_6m_5y_fv = 8, vaxigrip_6m_5y_ma = 8,
                               fluarix_6m_5y_ma = 6, fluquadri_6m_5y_ma = 10,
                               influvac_6m_5y_ma = 8, flucelvax_6m_5y_ma = 8,
                               vaxigrip_more_5y_ma = 8, fluarix_more_5y_ma = 8,
                               afluaria_more_5y_ma = 11, fluquadri_more_5y_ma = 7,
                               influvac_more_5y_ma = 5, flucelvax_more_5y_ma = 11,
                               fluad_more_60y_ma = 9, fluzone_more_60y_ma = 9,
                               vaxigrip_pregnant_ma = 3, fluarix_pregnant_ma = 3,
                               afluaria_pregnant_ma = 6, fluquadri_pregnant_ma = 3,
                               influvac_pregnant_ma = 6, flucelvax_pregnant_ma = 6))



cusum(y = events, n = records,
                      p_E = cusum_pars$expected[sgp], p_M = cusum_pars$maximum[sgp],
                      y_0 = cusum_pars$control[sgp]/2, C = cusum_pars$control[sgp])$X -> Y
cusum_pars$expected[sgp]
cusum_pars$maximum[sgp]
cusum_pars$control[sgp]
 
x <- vector(length = length(events))
class(x) <- "Date"

for(i in 1:length(x)) x[i] = data_date - 7*i
x <- rev(x)  
x <- x+7
Y <- Y[-1]
dat <- data.frame(x, Y)

x
ggplot(dat, aes(x=x, y=Y)) + geom_line() + geom_hline(aes(yintercept=cusum_pars$control[sgp]), col = "red") + ylab("CUSUM") + xlab("Date") +ggtitle("CUSUM Control chart for FT 6m - <5yrs Fever")
                                    