####Time series analysis for sta tp out put only (2006-2020)

#Packages
install.packages("itsmr")
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



#test for autocorrelation####

#tp_out
acf(sta2_int$out_tp_c_int)
pacf(sta2_int$out_tp_c_int)
Box.test(sta2_int$out_tp_c_int, lag=10, type="Ljung-Box")

#auto arima
auto.arima(sta2_int$out_tp_c_int, trace=T)# Best model: ARIMA(0,0,1) with non-zero mean 

#does it fix autocorrelation issues? YES!
fit <- Arima(sta2_int$out_tp_c_int, order=c(0,0,1))
pacf(residuals(fit))
acf(residuals(fit))

Box.test(residuals(fit), lag=10, type="Ljung-Box")

plot(residuals(fit))


#tp_IN
acf(sta2_int$in_tp_c_int)
pacf(sta2_int$in_tp_c_int)
Box.test(sta2_int$in_tp_c_int, lag=10, type="Ljung-Box")

#auto arima
auto.arima(sta2_int$in_tp_c_int, trace=T)# Best model: ARIMA(5,0,0) with non-zero mean 

#does it fix autocorrelation issues? YES!
fit <- Arima(sta2_int$in_tp_c_int, order=c(5,0,0))
in_arima<-residuals(fit)
pacf(residuals(fit))
acf(residuals(fit))
Box.test(residuals(fit), lag=10, type="Ljung-Box")


plot(residuals(fit))



#tp_RR
acf(sta2_int$tp_rr_int)
pacf(sta2_int$tp_rr_int)
Box.test(sta2_int$tp_rr_int, lag=10, type="Ljung-Box")

#auto arima
auto.arima(log1p(sta2_int$tp_rr_int), trace=T)# Best model: ARIMA(0,0,1) with non-zero mean 

#does it fix autocorrelation issues? YES!
fit <- Arima(log1p(sta2_int$tp_rr_int), order=c(2,0,1))
pacf(residuals(fit))
acf(residuals(fit))

####test of stationarity
Box.test(residuals(fit), lag=10, type="Ljung-Box")#want high pvalue
tseries::adf.test(residuals(fit))#want low opvalue
tseries::kpss.test(residuals(fit), null="Trend")#want high pvalue
plot(residuals(fit))


# full model arima?

Ccf(sta2_int$in_tp_c_int,sta2_int$out_tp_c_int)#use lags

#tp_RR

diff_out<-diff(sta2_int$out_tp_c_int)#difference response varible
tp_in<-sta2_int$in_tp_c_int[-1]#remove first observation to match diff y var above

#regular model model
tp_out <-gls(log(out_tp_c_int) ~  in_tp_c_int, data = sta2_int)
summary(tp_out)
plot(tp_out)

#differenced model
tp_out_diff <-gls(diff_out ~  tp_in, data = sta2_int)

summary(tp_out_diff)
plot(tp_out_diff)#check residuals


#test stationarity replace whatever model you are looking re: diff or not
pacf(residuals(tp_out_diff))
acf(residuals(tp_out_diff))
Box.test(residuals(tp_out), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(tp_out_diff)) #want low pvalue
tseries::kpss.test(residuals(tp_out), null="Trend") #want high pvalue



#test p, q for corARMA ####
cor.results <- NULL
for(i in 0:5) {
  for(j in 0:5) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(diff_out  ~ 
                          tp_in, 
                        correlation = corARMA(p = i, q = j,form =~ 1),
                        data = sta2_int, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')#Regression with ARIMA(1,0,0) same result as auto arima
cor.results %>% arrange(AIC)


#auto arima
auto.arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=sta2_int$in_tp_c_int, trace=T)

(fit <- Arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=sta2_int$in_tp_c_int, order=c(1,0,0), seasonal=c(1,0,0)))
#test coefficients
coeftest(fit)

pacf(residuals(fit))





(fit <- Arima(log(sta2_int$out_tp_c_int), xreg=sta2_int$in_tp_c_int, order=c(1,0,0)))
#test coefficients
coeftest(fit)

pacf(residuals(fit))


tp_out_ARMA <-gls(log(out_tp_c_int) ~ 
                    in_tp_c_int,
                  data=sta2_int,
                  correlation = corARMA(p = 1, q = 0))



pacf(residuals(tp_out_ARMA, type = "normalized"))

pacf(residuals(tp_out_ARMA))

plot(residuals(tp_out_ARMA, type = "normalized"),residuals(fit))


#best model 
residuals(tp_out_ARMA, type = "normalized")
tp_out_ARMA <-gls(log(out_tp_c_int) ~ 
                    in_tp_c_int, 
                  correlation = corARMA(p = 1, q = 0),
                  data = sta2_int, 
                  na.action = na.exclude)

summary(tp_out_ARMA)

#does it fix autocorrelation issues? YES!
pacf(residuals(tp_out_ARMA))
acf(residuals(tp_out_ARMA))




#make actual timeseries object and decompose####

#time series for tp out
sta2_ts<-ts(sta2_int$out_tp_c_int, frequency = 12)#note that May is the first month
plot.ts(sta2_ts)
sta_comps<-decompose(sta2_ts)#decompose into trend, season and noise
plot(sta_comps)#plot decomposition components

#check for correlations
pacf(sta2_ts)
acf(sta2_ts)


#check best fit arima model
auto.arima(sta2_ts)#ARIMA(0,0,1)(1,0,0)[12] with non-zero mean 

#fit model

(fit <- Arima(sta2_ts, order=c(0,0,1), seasonal=c(1,0,0)))

pacf(residuals(fit))
acf(residuals(fit))

#remove seasonality

sta2_seas_adj <- sta2_ts-sta_comps$seasonal

#decompose and plot to make sure season is gone, it is, I think
sta_comps<-decompose(sta2_seas_adj)#decompose into trend, season and noise
plot(sta_comps)

plot(sta2_seas_adj)
pacf(as.numeric(sta2_seas_adj))
acf(as.numeric(sta2_seas_adj))

#check best fit auto.arima
auto.arima(sta2_seas_adj)#ARIMA(0,0,1)(0,0,2)[12] with non-zero mean 

(fit <- Arima(sta2_seas_adj, order=c(0,0,1), seasonal=c(0,0,2)))

pacf(as.numeric(residuals(fit)))
acf(as.numeric(residuals(fit)))

diff<-diff(sta2_seas_adj,difference=1)
plot.ts(diff)
pacf(diff,lag.max=20)
acf(diff,lag.max=20)

auto.arima(sta2_ts)


#seasonal = c(0, 0, 0)



(fit <- Arima(sta2$out_tp_c, order=c(0,0,2)))
acf(residuals(fit))
