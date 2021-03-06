####STA 2 PIECEWISE MODELS

#Packages####

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
NA_dat<-read.csv("data/4_sta_4_16.csv", row=1) #data produced by the clean_data.R file

head(NA_dat)


#subset for sta2
sta2<-subset(NA_dat,sta %in% c("sta_2"))


#select variables used in model 
mod_vars<-sta2 %>%
  select(out_tp_c,in_tp_c,tp_rr,in_tn_c,tn_rr,in_ca_c,ca_rr,in_water_l,temp_mean, rainfall_mean,sta,por2, month, year, HRT, wd_mean, out_water_l, wd_0_mean)



#different code to remove outliers; needs to be adapted to our data

#out <- boxplot.stats(dat$hwy)$out
#out_ind <- which(dat$hwy %in% c(out))
#out_ind


#lower_bound <- quantile(dat$hwy, 0.025)
#lower_bound

#upper_bound <- quantile(dat$hwy, 0.975)
#upper_bound

#outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
#outlier_ind

#remove the outlier/error from the tp_out (point 225)
#p_out_dat<- full_dat %>%
#filter(out_tp_c < 0.2)


#interpolate using splines for each variable. some don't have nas and don't need it
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
  mutate(HRT_int=na.spline(sta2$HRT, sta2$por2))%>%             #6 NA
  mutate(wd_mean_int=na.spline(sta2$wd_mean, sta2$por2))%>%     #6NA
  mutate(wd_0_mean_int=na.spline(sta2$wd_0_mean, sta2$por2))%>% #no na
  mutate(out_water_int=na.spline(sta2$out_water_l, sta2$por2))  #no na

#sta2_int$HRT_int[sta2_int$HRT_int<0] <- 0 #replace ridiculous negative interpolated value with 0

# calculate the # of na's for given variable
#count(is.na(subset(mod_vars,sta %in% c("sta_2"))$out_water_l))

#### Model "piece" predicting Total p outflow conc. (out_tp_c_int )####

#Histograms of variables in the model. looking for outliers

hist(sta2_int$out_water_int,  breaks = 1000) # value of 0.643951 for data point 225 seems like a data error
boxplot(sta2_int$out_water_l)

hist(sta2_int$in_tp_c_int,  breaks = 1000) # 
boxplot(sta2_int$in_tp_c_int)

hist(sta2_int$tp_rr_int,  breaks = 1000) # 
boxplot(sta2_int$tp_rr_int)




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

tp_out <-gls(log(out_tp_c_int) ~  in_tp_c_int+tp_rr_int+out_water_int+in_ca_c_int+in_tn_c_int, data = sta2_int)

summary(tp_out)# summary for p-values
rsquared(tp_out)#r squared value
plot(tp_out) #look at residual plot to assess model fit
car::vif(tp_out) #variance inflation factor to look for collinearity

#test stationarity 
pacf(residuals(tp_out))#lag of 1
acf(residuals(tp_out))
Box.test(residuals(tp_out), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_out)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_out), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$in_tp_c_int,sta2_int$tp_rr_int,sta2_int$out_water_int, sta2_int$in_tn_c_int,sta2_int$in_ca_c_int)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","TP_RR","water_out","TN_IN","TC_IN" )#give them names

auto.arima(log(ts(sta2_int$out_tp_c_int, frequency = 12)), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(log(sta2_int$out_tp_c_int), xreg=x_vars , trace=T)# Best model: Regression with ARIMA(1,0,0)

##test p, q with corARMA manually with a loop
cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log(out_tp_c_int)  ~ 
                          in_tp_c_int+
                          tp_rr_int+
                          in_tn_c_int+
                          in_ca_c_int+
                          out_water_int,  
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
#1 0 104.4744
#0 2 105.9689


#without season--this seems fine...
Arima_fit2 <- Arima(log(sta2_int$out_tp_c_int), xreg=x_vars, order=c(1,0,0))
summary(Arima_fit2)

#look at model results and fit
coeftest(Arima_fit2)# get pvalues


#test stationarity 
pacf(residuals(Arima_fit2))#looks good
acf(residuals(Arima_fit2))#looks good
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit2)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue


#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log(out_tp_c_int)  ~ 
                 in_tp_c_int+
                 log1p(tp_rr_int)+
                 in_tn_c_int+
                 in_ca_c_int+
                 out_water_int,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)

#assess model
summary(ARMA_fit)#get pvalues
rsquared(ARMA_fit)
plot(ARMA_fit)#look at residual plot
car::vif(ARMA_fit)#look for collinearity

#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#looks good
acf(residuals(ARMA_fit, type="normalized"))#looks good
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
                 log1p(tp_rr_int)+
                 in_tn_c_int+
                 in_ca_c_int+
                 out_water_int,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)

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

hist(sta2_int$HRT,  breaks = 1000) # 
boxplot(sta2_int$HRT)




#remove the outlier/error from the tp_out (point 225)
#p_out_dat<- full_dat %>%
#filter(out_tp_c < 0.2) 



#GLS model without temporal correlation structure (note: with collinear predictors)

tp_RR <-gls(log1p(tp_rr_int) ~  
              in_tp_c_int + 
              in_ca_c_int+
              in_tn_c_int+
              por2 + 
              in_water_l_int+
             wd_0_mean_int, 
            data = sta2_int)




summary(tp_RR)#get pvalues
car::vif(tp_RR)# check variance inflation, looks ok
rsquared(tp_RR)
plot(tp_RR)#look at residual plot, looks ok


#looking at univartiate models
tp_RR <-gls(log1p(tp_rr_int) ~  in_tp_c_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  in_ca_c_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~   in_tn_c_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  in_water_l_int , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~  por2 , data = sta2_int)
tp_RR <-gls(log1p(tp_rr_int) ~ wd_0_mean_int, data = sta2_int)

#look at univarite results
summary(tp_RR)
rsquared(tp_RR)
plot(tp_RR)

#test stationarity 
pacf(residuals(tp_RR))#has lags
acf(residuals(tp_RR))
Box.test(residuals(tp_RR), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_RR)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_RR), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$in_tp_c_int,sta2_int$in_ca_c_int, sta2_int$in_tn_c_int, sta2_int$in_water_l_int, sta2_int$por2)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","CA_IN","TN_IN", "IN_H2o", "por"  )#give them names

cor(x_vars)#correlation values for vars
pairs(x_vars)#pairwise plots for vars

#run auto.arima
auto.arima(log1p(ts(sta2_int$tp_rr_int, frequency = 12)), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(0,0,1)(1,0,1)[12] errors 
auto.arima(log1p(sta2_int$tp_rr_int), xreg=x_vars , trace=T, stepwise=FALSE,approx=FALSE) # Best model: Regression with ARIMA(0,0,5)



#without season--this seems fine...
Arima_fit2 <- Arima(log1p(sta2_int$tp_rr_int),  xreg=x_vars,order=c(0,0,5))
summary(Arima_fit2)


#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#looks good
acf(residuals(Arima_fit2))#looks good
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue


###test p, q with corARMA manually with a loop
cor.results <- NULL
for(i in 0:5) {
  for(j in 0:5) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log1p(tp_rr_int)  ~ 
                          in_tp_c_int+
                          in_ca_c_int+
                          in_tn_c_int+ 
                          in_water_l_int+
                          por2,
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int, 
                        method="ML")
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')#gives diff result than auto arima?...
cor.results %>% arrange(AIC) 

#  i j        AIC
#  3 2 -113.80047
#  0 5 -111.92249

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log1p(tp_rr_int)  ~ 
                 in_tp_c_int+
                 in_ca_c_int+
                 in_tn_c_int+ 
                 in_water_l_int+
                 por2,
               correlation = corARMA(p = 3, q = 2),
               data = sta2_int, 
               method="ML")

AIC(ARMA_fit)

#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)



#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#fine
acf(residuals(ARMA_fit, type="normalized"))#fine
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok, wierd tail...
hist(E1)



#final model to pass on to piecewiseSEM

TPC_RR <-gls(log1p(tp_rr_int)  ~ 
               in_tp_c_int+
               in_ca_c_int+
               in_tn_c_int+ 
               in_water_l_int+
               por2,
             correlation = corARMA(p = 3, q = 2),
             data = sta2_int, 
             method="ML")



#### Model "piece" predicting Water depth####

#plot time series



#hydraulic inflow
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=in_water_l_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Inflow Hydraulic loading rate")

sep_sta_2



#depth
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=wd_0_mean_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("mean water depth above zero")

sep_sta_2

####GLS model without temporal correlation structure 

wd_0 <-gls(wd_0_mean_int~  
             in_water_l_int,
           data = sta2_int)


#look at model results
summary(wd_0)
rsquared(wd_0)
AIC(wd_0)
plot(wd_0)#look at residual plot, looks ok



#test stationarity 
pacf(residuals(wd_0))#lag of 1
acf(residuals(wd_0))
Box.test(residuals(wd_0), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(wd_0)) #want pvalue <0.05
tseries::kpss.test(residuals(wd_0)) #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
#x_vars<-cbind( sta2_int$in_water_l_int, sta2_int$por2, sta2_int$rainfall_mean_int)#create data frame with predictor variables
#colnames(x_vars)<-c( "IN_H2o", "por","precip" )#give them names

rs

#run auto.arima
auto.arima(ts(sta2_int$wd_0_mean_int, frequency = 12), xreg=sta2_int$in_water_l , trace=T) # Best model: Regression with ARIMA(5,1,1)(2,0,1)[12] errors 
auto.arima(sta2_int$wd_0_mean_int, xreg=sta2_int$in_water_l , trace=T) # Best model: Regression with ARIMA(1,0,0)



#without season. seems to need season...
Arima_fit2 <- Arima(sta2_int$wd_0_mean_int, xreg=sta2_int$in_water_l, order=c(5,1,1))
summary(Arima_fit2)



#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#ok looks like 12 month lag...
acf(residuals(Arima_fit2))
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue




cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(wd_0_mean_int~  
                          in_water_l_int,
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)# ?

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(wd_0_mean_int~  
                 in_water_l_int,
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)



#check out model
summary(ARMA_fit)
rsquared(ARMA_fit)




#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#12 month...
acf(residuals(ARMA_fit, type="normalized"))#12 month...
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks not great...
hist(E1)



#final model to pass on to piecewiseSEM
#residuals are heteroskedastic and non-normal... this model needs help...

wd_0 <-gls(wd_0_mean_int~  
             in_water_l_int,
           correlation = corARMA(p = 1, q = 0),
           data = sta2_int)



#### Model "piece" predicting hydraulic outflow####

#plot time series

#hydraulic outflow
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=out_water_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("Outflow Hydraulic loading rate")

sep_sta_2



#depth
sep_sta_2<-ggplot(sta2_int, aes(x=por2, y=wd_0_mean_int)) +
  geom_line(aes(color=sta))+xlab("Period of Record (Month)") + 
  ylab("mean water depth above zero")

sep_sta_2

####GLS model without temporal correlation structure 

water_out <-gls(out_water_int~
             wd_0_mean_int,
           data = sta2_int)


#car::vif(wd_0)# check variance inflation, looks ok


summary(water_out)
rsquared(water_out)
AIC(wd_0)
plot(wd_0)#look at residual plot, looks ok



#test stationarity 
pacf(residuals(water_out))#lag of 1
acf(residuals(water_out))
Box.test(residuals(water_out), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(water_out)) #want pvalue <0.05
tseries::kpss.test(residuals(water_out)) #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
#x_vars<-cbind( sta2_int$in_water_l_int, sta2_int$por2, sta2_int$rainfall_mean_int)#create data frame with predictor variables
#colnames(x_vars)<-c( "IN_H2o", "por","precip" )#give them names

#run auto.arima
auto.arima(ts(sta2_int$out_water_int, frequency = 12), xreg=sta2_int$wd_0_mean_int , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(sta2_int$out_water_int, xreg=sta2_int$wd_0_mean_int , trace=T) # Best model: Regression with ARIMA(1,0,0)
 


#without season. seems to need season...
Arima_fit2 <- Arima(sta2_int$out_water_int, xreg=sta2_int$wd_0_mean_int, order=c(0,1,0))
summary(Arima_fit2)



#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#ok looks like 12 month lag...
acf(residuals(Arima_fit2))
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue



#test by loop and AIC
cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(out_water_int~  
                          wd_0_mean_int,
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)# ?

#   i j      AIC
#1  0 2 1418.711
#2  2 0 1418.780
#3  1 1 1419.230
#4  0 1 1419.521

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log1p(out_water_int)~  
                 wd_0_mean_int,
               correlation = corARMA(p = 0, q = 2),
               data = sta2_int)



#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)



#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#looks good
acf(residuals(ARMA_fit, type="normalized"))##looks good
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks not great...
qqnorm(E1)#qqplot
qqline(E1) # looks ok
hist(E1)



#final model to pass on to piecewiseSEM
#residuals are heterosjedastic and non-normal... this model needs help...
OUT_H20 <-gls(log1p(out_water_int)~  
             wd_0_mean_int,
           correlation = corARMA(p = 0, q = 2),
           data = sta2_int)