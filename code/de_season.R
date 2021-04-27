# Load required libraries
library(nlme)
library(piecewiseSEM)
library(semPlot)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

#Import data####
NA_dat<-read.csv("data/4_sta_4_16.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(NA_dat)


#subset for sta2
sta2<-subset(NA_dat,sta %in% c("sta_2"))


#select variables used in model (missing VEG as of 4/26/21; check for others)
mod_vars<-sta2 %>%
  select(out_tp_c,in_tp_c,tp_rr,in_tn_c,tn_rr,in_ca_c,ca_rr,in_water_l,temp_mean, rainfall_mean,sta,por2, month, year, HRT, HRT_365)


#interpolate using splines for each variable. some don;t have nas and dont need it
sta2_int<-mod_vars%>%
  mutate(out_tp_c_int=na.spline(sta2$out_tp_c,sta2$por2))%>%   #interpolate over 6 nas
  mutate(in_tp_c_int=na.spline(sta2$in_tp_c,sta2$por2))%>%     #interpolate over 1 na
  mutate(tp_rr_int=na.spline(sta2$tp_rr,sta2$por2))%>%         #no na
  mutate(in_tn_c_int=na.spline(sta2$in_tn_c,sta2$por2))%>%     #1 na
  mutate(tn_rr_int=na.spline(sta2$tn_rr,sta2$por2))%>%         #no na
  mutate(in_ca_c_int=na.spline(sta2$in_ca_c,sta2$por2))%>%     #1 na
  mutate(ca_rr_int=na.spline(sta2$ca_rr,sta2$por2))%>%         #no na
  mutate(in_water_l_int=na.spline(sta2$in_water_l,sta2$por2))%>%#no na
  mutate(temp_mean_int=na.spline(sta2$temp_mean,sta2$por2))%>% #no na
  mutate(rainfall_mean_int=na.spline(sta2$rainfall_mean,sta2$por2))%>%#no na
  mutate(HRT_int=na.spline(sta2$HRT, sta2$por2))%>%#6 NA
  mutate(HRT365_int=na.spline(sta2$HRT_365, sta2$por2))#6NA

sta2_int$HRT_int[sta2_int$HRT_int<0] <- 0#replace ridiculous neg interpolated value with 0




#Remove season from each variable that has a seasonal component####

#total P out (out_tp_c)

#make time series object
tp_ts<-ts(sta2_int$out_tp_c_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tp_out_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tp_out_seas)

#total P in(tp_c_in)

#make time series object
tp_ts<-ts(sta2_int$in_tp_c_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tp_in_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tp_in_seas)

#total P retention rate (tp_rr_in)

#make time series object
tp_ts<-ts(sta2_int$tp_rr_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tp_rr_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tp_rr_seas)


#total n inflow (in_tn_c_int)

#make time series object
tp_ts<-ts(sta2_int$in_tn_c_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tn_in_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tn_in_seas)


#total n retention rate (tn_rr_in)

#make time series object
tp_ts<-ts(sta2_int$tn_rr_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tn_rr_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tn_rr_seas)

#total CA inflow (in_ca_c_int)

#make time series object
tp_ts<-ts(sta2_int$in_ca_c_in, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tca_in_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tca_in_seas)


#total CA retention rate(ca_rr_int)

#make time series object
tp_ts<-ts(sta2_int$ca_rr_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

tca_rr_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(tca_rr_seas)


#hydrologic loading (in_water_l_int)
#make time series object
tp_ts<-ts(sta2_int$in_water_l_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

in_water_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(in_water_seas )

#temperature (temp_mean_int)

tp_ts<-ts(sta2_int$temp_mean_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

temp_mean_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(temp_mean_seas)


#precip (rainfall_mean_intt)

tp_ts<-ts(sta2_int$rainfall_mean_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

rain_mean_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(rain_mean_seas)



#Hydraulic retention rate (HRT_int)

tp_ts<-ts(sta2_int$HRT_int, frequency = 12)#note that May is the first month
plot.ts(tp_ts)
sta_comps<-decompose(tp_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

HRT_seas <- tp_ts-sta_comps$seasonal #remove seasonal component
plot.ts(HRT_seas)

#### add to data set
sta2_int<-mod_vars%>%
  mutate(tp_out_seas=tp_out_seas)%>% 
  mutate(tp_in_seas=tp_in_seas)%>%  
  mutate(tp_rr_seas=tp_rr_seas)%>% 
  mutate(tn_in_seas=tn_in_seas)%>% 
  mutate(tn_rr_seas=tn_rr_seas)%>% 
  mutate(tca_in_seas=tca_in_seas)%>% 
  mutate(tca_rr_seas=tca_rr_seas)%>% 
  mutate(in_water_seas=in_water_seas)%>% 
  mutate(temp_mean_seas=temp_mean_seas)%>% 
  mutate(rain_mean_seas=rain_mean_seas)%>% 
  mutate(HRT_seas=HRT_seas)