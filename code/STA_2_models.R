####STA 2 PIECEWISE MODELS

#Packages####
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

#Import data####
NA_dat<-read.csv("data/4_sta.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(NA_dat)


#subset for sta2
sta2<-subset(NA_dat,sta %in% c("sta_2"))


#select variables used in model (missing HRT, VEG as of 4/15/21; check for others)
mod_vars<-sta2 %>%
  select(out_tp_c,in_tp_c,tp_rr,in_tn_c,tn_rr,in_ca_c,ca_rr,in_water_l,temp_mean, rainfall_mean,sta,por2, month, year)


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
  mutate(rainfall_mean_int=na.spline(sta2$rainfall_mean,sta2$por2))#no na

# calculate the # of na's for given variable
count(is.na(subset(mod_vars,sta %in% c("sta_2"))$in_tn_c))#of nas in out_tp_c

#### Model "piece" predicting Total p outflow conc. (out_tp_c_int )####
#tp OUTPUT= tp INPUT +TP retention rate

#Histograms of variables in the model. looking for outliers

hist(sta2_int$out_tp_c_int,  breaks = 1000) # value of 0.643951 for data point 225 seems like a data error
boxplot(sta2_int$out_tp_c_int)

hist(sta2_int$in_tp_c_int,  breaks = 1000) # 
boxplot(sta2_int$in_tp_c_int)

hist(sta2_int$tp_rr_int,  breaks = 1000) # 
boxplot(sta2_int$tp_rr_int)


#remove the outlier/error from the tp_out (point 225)
#p_out_dat<- full_dat %>%
#filter(out_tp_c < 0.2)  


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



#GLS model without temporal correlation structure (note: with collinear predictors)

tp_out <-gls(log(out_tp_c_int) ~  in_tp_c_int + tp_rr_int, data = sta2_int)
summary(tp_out)
rsquared(tp_out)
plot(tp_out)#look at residual plot

#test stationarity 
pacf(residuals(tp_out))#lag of 1
acf(residuals(tp_out))
Box.test(residuals(tp_out), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_out)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_out), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$in_tp_c_int,sta2_int$tp_rr_int)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","TP_RR" )#give them names
auto.arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 

##test p, q with corARMA manually with a loop
cor.results <- NULL
for(i in 0:5) {
  for(j in 0:5) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log(out_tp_c_int)  ~ 
                        in_tp_c_int+
                        tp_rr_int,  
                        correlation = corARMA(p = i, q = j,form =~ 1),
                        data = sta2_int, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

#results
colnames(cor.results) <- c('i', 'j', 'AIC')#Regression with ARMA(1,0) is barely the best: same result as auto arima
cor.results %>% arrange(AIC)

#best fit
#i j      AIC
#1 1 0 80.06377

#run best fit model with arima (NOTE THAT EACH PREDICTOR BY ITSELF has the same corrolation structure (1,0,0))

#with season
Arima_fit <- Arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=x_vars, order=c(1,0,0), seasonal=c(1,0,0))
summary(Arima_fit)

#test coefficients
coeftest(Arima_fit)

#without season--this seems fine...
Arima_fit2 <- Arima(log(sta2_int$out_tp_c_int), xreg=x_vars, order=c(1,0,0))
summary(Arima_fit2)

#test coefficients
coeftest(Arima_fit2)

#test stationarity 
pacf(residuals(Arima_fit2))#lag of 1
acf(residuals(Arima_fit2))
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit2)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue


#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log(out_tp_c_int)  ~ 
                    in_tp_c_int+
                    tp_rr_int,  
                  correlation = corARMA(p = 1, q = 0,form =~ 1),
                  data = sta2_int, 
                  na.action = na.exclude)


summary(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)

#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#lag of 1
acf(residuals(ARMA_fit, type="normalized"))
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok



#final model to pass on to piesewiseSEM

TP_out_c <-gls(log(out_tp_c_int)  ~ 
                 in_tp_c_int+
                 tp_rr_int,  
               correlation = corARMA(p = 1, q = 0,form =~ 1),
               data = sta2_int, 
               na.action = na.exclude)

#### Model "piece" predicting Phosphorus Retention Rate####
#Histograms of variables in the model. looking for outliers

hist(sta2_int$in_water_l_int,  breaks = 1000) # value of 0.643951 for data point 225 seems like a data error
boxplot(sta2_int$in_water_l_int)

hist(sta2_int$in_tp_c_int,  breaks = 1000) # 
boxplot(sta2_int$in_tp_c_int)

hist(sta2_int$tp_rr_int,  breaks = 1000) # 
boxplot(sta2_int$tp_rr_int)

hist(sta2_int$tn_rr_int,  breaks = 1000) # 
boxplot(sta2_int$tn_rr_int)


hist(sta2_int$ca_rr_int,  breaks = 1000) # 
boxplot(sta2_int$ca_rr_int)

hist(sta2_int$por2,  breaks = 1000) # 
boxplot(sta2_int$por2)


#remove the outlier/error from the tp_out (point 225)
#p_out_dat<- full_dat %>%
#filter(out_tp_c < 0.2)  


#plot time series

#ca_rr
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=ca_rr_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Total Ca retention rate")

sep_sta_2


#tn_rr
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=tn_rr_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Total N retention rate")

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


#i=hydraulic inflow
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=in_water_l_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Inflow Hydraulic loading rate")

sep_sta_2



#GLS model without temporal correlation structure (note: with collinear predictors)

tp_RR <-gls(log1p(tp_rr_int) ~  in_tp_c_int + ca_rr_int+  tn_rr_int+ por2 + in_water_l_int+temp_mean_int, data = sta2_int)

car::vif(tp_RR)# check variance inflation, looks ok
summary(tp_RR)
rsquared(tp_RR)
plot(tp_RR)#look at residual plot, looks ok


#looking at univartiate models
tp_RR <-gls(log1p(tp_rr_int) ~  in_tp_c_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  ca_rr_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  tn_rr_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  in_water_l_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  por2 , data = sta2_int)
plot( sta2_int$tn_rr_int, log1p(sta2_int$tp_rr_int) )

#test stationarity 
pacf(residuals(tp_RR))#lag of 1
acf(residuals(tp_RR))
Box.test(residuals(tp_RR), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_RR)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_RR), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$in_tp_c_int,sta2_int$ca_rr_int, sta2_int$tn_rr_int, sta2_int$in_water_l_int, sta2_int$por2, sta2_int$temp_mean_int)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","CA_RR","TN_RR", "IN_H2o", "por","temp" )#give them names
auto.arima(log1p(ts(sta2_int$tp_rr_int, frequency = 12)), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(log1p(sta2_int$tp_rr_int), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 

##test p, q with corARMA manually with a loop
cor.results <- NULL
for(i in 0:5) {
  for(j in 0:5) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log(out_tp_c_int)  ~ 
                          in_tp_c_int+
                          tp_rr_int,  
                        correlation = corARMA(p = i, q = j,form =~ 1),
                        data = sta2_int, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

#results
colnames(cor.results) <- c('i', 'j', 'AIC')#Regression with ARMA(1,0) is barely the best: same result as auto arima
cor.results %>% arrange(AIC)

#best fit
#i j      AIC
#1 1 0 80.06377

#run best fit model with arima (NOTE THAT EACH PREDICTOR BY ITSELF has the same corrolation structure (1,0,0))

#with season
Arima_fit <- Arima(log1p(ts(sta2_int$tp_rr_int, frequency = 12)), xreg=x_vars, order=c(0,0,1), seasonal=c(1,0,1))
summary(Arima_fit)

#test coefficients
coeftest(Arima_fit)

#without season--this seems fine...
Arima_fit2 <- Arima(log1p(sta2_int$tp_rr_int), xreg=x_vars, order=c(0,0,2))
summary(Arima_fit2)

#test coefficients
coeftest(Arima_fit2)

#test stationarity 
pacf(residuals(Arima_fit2))#lag of 1
acf(residuals(Arima_fit2))
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue


#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log1p(tp_rr_int)  ~ 
                 in_tp_c_int+
                 ca_rr_int+
                 tn_rr_int+ 
                 in_water_l_int+
                 por2+
                 temp_mean_int,  
               correlation = corARMA(p = 0, q = 2,form =~ 1),
               data = sta2_int, 
               na.action = na.exclude)


summary(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)

#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#lag of 1
acf(residuals(ARMA_fit, type="normalized"))
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok



#final model to pass on to piesewiseSEM

TP_out_c <-gls(log(out_tp_c_int)  ~ 
                 in_tp_c_int+
                 tp_rr_int,  
               correlation = corARMA(p = 1, q = 0,form =~ 1),
               data = sta2_int, 
               na.action = na.exclude)