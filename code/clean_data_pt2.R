library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)


#interpolate all varaibles for sta2 with spline

NA_dat<-read.csv("data/4_sta.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(NA_dat)

mod_vars<-NA_dat %>%
  select(out_tp_c,in_tp_c,tp_rr,in_tn_c,tn_rr,in_ca_c,ca_rr,in_water_l,temp_mean, rainfall_mean,sta,por2, month, year) 