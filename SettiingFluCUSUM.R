library(tidyverse)
library(here)
library(stringr)
library(validate)
library(beepr)
library(lubridate)
library(data.table)
library(tictoc)
library(janitor)
library(tidylog)


######################
#CUSUM ER and MR 2023
data_date <- as.Date("2022-09-05")
dat <- readRDS("Z:/Influenza/Data/Signal Detection/2022-09-05_data_sd.Rds")

table(dat$VAX_BRAND)


expected <- c(vaxigrip_6m_5y_fv = 0.042, fluarix_6m_5y_fv = 0.042,
             fluquadri_6m_5y_fv = 0.032, influvac_6m_5y_fv = 0.042,
             flucelvax_6m_5y_fv = 0.042, vaxigrip_6m_5y_ma = 0.007,
             fluarix_6m_5y_ma = 0.007, fluquadri_6m_5y_ma = 0.006,
             influvac_6m_5y_ma = 0.007, flucelvax_6m_5y_ma = 0.007,
             vaxigrip_more_5y_ma = 0.004, fluarix_more_5y_ma = 0.004,
             afluaria_more_5y_ma = 0.004, fluquadri_more_5y_ma = 0.004,
             influvac_more_5y_ma = 0.007, flucelvax_more_5y_ma = 0.004,
             fluad_more_60y_ma = 0.003, fluzone_more_60y_ma = 0.003,
             vaxigrip_pregnant_ma = 0.006, fluarix_pregnant_ma = 0.006,
             afluaria_pregnant_ma = 0.008, fluquadri_pregnant_ma = 0.003,
             influvac_pregnant_ma = 0.008, flucelvax_pregnant_ma = 0.008)

#VT 6m - 5y Fever 2022 observed rate 10.3% >10000 responses 2022 ER = 4.2%
#FT 6m - 5y Fever 2022 observed rate 7.8%  >2000 responses 2022 ER = 4.2%
#FQ 6m - 5y Fever 2022 observed rate 0.5% 757 responses 2022 ER = 3.2%
#Inf 6m - 5y Fever 2022


dat$group <- interaction(dat$vax_brand,dat$age_group_cusum, dat$type,drop = TRUE)

dat <- as.data.table(dat)

dat[,.(prop = mean(event),n = .N), by = group]

#MA

#FEVER