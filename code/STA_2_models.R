####STA 2 PIECEWISE MODELS

#Packages
#install.packages("itsmr")
library(itsmr)
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
library(forecast)
library(piecewiseSEM)

#import data####

NA_dat<-read.csv("data/4_sta.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(NA_dat)

full_dat<-NA_dat %>%
  select(out_tp_c,in_tp_c,tp_rr, sta,por2) 
#%>%filter(complete.cases(.))


####build model "piece" predicting total p outflow conc. (out_tp_c )####
#tp OUTPUT= tp INPUT +TP retention rate

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
#not this is just for this model and needs to be done for all other variables in the path diagram (i.e.,no-na's)
sta2_int<-sta2%>%
  mutate(out_tp_c_int=na.spline(sta2$out_tp_c,sta2$por2))%>%#interpolate over 6 nas
  mutate(in_tp_c_int=na.spline(sta2$in_tp_c,sta2$por2))%>%#interpolate over 1 na
  mutate(tp_rr_int=na.spline(sta2$tp_rr,sta2$por2))#no na

#of nas in given variable
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


####full model (with colinear predictors)####
#regular model model

tp_out <-gls(log(out_tp_c_int) ~  in_tp_c_int + tp_rr_int, data = sta2_int)
summary(tp_out)
rsquared(tp_out)
plot(tp_out)

#test stationarity 
pacf(residuals(tp_out))#lag of 1
acf(residuals(tp_out))
Box.test(residuals(tp_out), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(tp_out)) #want low pvalue
tseries::kpss.test(residuals(tp_out), null="Trend") #want high pvalue

#Use auto.arima function
x_vars<-cbind(sta2_int$in_tp_c_int,sta2_int$tp_rr_int)
colnames(x_vars)<-c("TPC_IN","TP_RR" )
#there are the predictor variables for the model
auto.arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=x_vars , trace=T) 

           