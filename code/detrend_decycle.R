####Time series analysis for sta tp out put only (2006-2020)

#Packages

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(nlme)
library(lme4)
library(lmtest)
library(MuMIn) 
library(lme4)
library(zoo)

#import data####

NA_dat<-read.csv("data/4_sta.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(NA_dat)

full_dat<-NA_dat %>%
  select(out_tp_c,in_tp_c,tp_rr, sta,por2) 
#%>%filter(complete.cases(.))

#???????????????????remove na before everything?

####build model "piece" predicting total p outflow conc. (out_tp_c )####


#Histograms of variables in the model. looking for outliers
hist(full_dat$out_tp_c,  breaks = 1000) # value of 0.643951 for data point 225 seems like a data error
boxplot(full_dat$out_tp_c)

hist(full_dat$in_tp_c,  breaks = 1000) # 
boxplot(full_dat$in_tp_c)

hist(full_dat$tp_rr,  breaks = 1000) # 
boxplot(full_dat$tp_rr)


#remove the outlier/error from the tp_out (point 225)
#p_out_dat<- full_dat %>%
#filter(out_tp_c < 0.2)  




#look time series for STA2
#subset for sta2
sta2<-subset(full_dat,sta %in% c("sta_2"))


#interpolate using splines for each varaible
sta2_int<-sta2%>%
      mutate(out_tp_c_int=na.spline(sta2$out_tp_c,sta2$por2))%>%#interpolate over 6 nas
      mutate(in_tp_c_int=na.spline(sta2$in_tp_c,sta2$por2))%>%#interpolate over 1 na
      mutate(tp_rr_int=na.spline(sta2$tp_rr,sta2$por2))#no na

#of nas in out_tp_c
count(is.na(subset(full_dat,sta %in% c("sta_2"))$tp_rr))#of nas in out_tp_c

#plot time series

#tp_out
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=out_tp_c_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Total [P] output")

sep_sta_2

#tp_in
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=in_tp_c_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Total [P] input")

sep_sta_2


#tp_rr
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=tp_rr_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Total P retention rate")

sep_sta_2






#test for autocorrelation
acf(sta2_int$out_tp_c_int)
pacf(sta2_int$out_tp_c_int)
Box.test(sta2_int$out_tp_c_int, lag=20, type="Ljung-Box")




#auto arima
auto.arima(sta2_int$out_tp_c_int, trace=T)

fit <- Arima(sta2_int$out_tp_c_int, order=c(0,0,1)))
pacf(residuals(fit))
acf(residuals(fit))

plot(residuals(fit))


tp_out <-lm(log(out_tp_c_int) ~  in_tp_c, data = sta2_int)
     
     
sta2_int$in_tp_c   
     
#make actual timeseries object####

sta2_ts<-ts(residuals(fit), frequency = 12)#note that May is the first month
plot.ts(sta2_ts)
sta_comps<-decompose(sta2_ts)#decompose into trend, season and noise

plot(sta_comps)#plot decomposition components



sta2_seas_adj <- sta2_ts-sta_comps$seasonal

plot(sta2_seas_adj)
acf(sta2_seas_adj)
diff<-diff(sta2_seas_adj,difference=1)

plot.ts(diff)
pacf(diff,lag.max=20)
acf(diff,lag.max=20)

auto.arima(sta2_ts)

(fit <- Arima(sta2$out_tp_c, order=c(0,0,2)))
acf(residuals(fit))
