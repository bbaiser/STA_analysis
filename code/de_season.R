# Load required libraries
library(nlme)
library(piecewiseSEM)
library(semPlot)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(zoo)
library(forecast)
library(lmtest)
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

#piecewise models####
#### Model "piece" predicting Phosphorus outflow####

tp_out <-gls(log(tp_out_seas) ~  tp_in_seas+ tp_rr_seas, data = sta2_int)
summary(tp_out)
rsquared(tp_out)
plot(tp_out)#look at residual plot
car::vif(tp_out)#

#test stationarity 
pacf(residuals(tp_out))#lag of 1
acf(residuals(tp_out))
Box.test(residuals(tp_out), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_out)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_out), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$tp_in_seas,sta2_int$tp_rr_seas)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","TP_RR" )#give them names
auto.arima(log(sta2_int$tp_out_seas), xreg=x_vars , trace=T) # 

##test p, q with corARMA manually with a loop
cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log(tp_out_seas)  ~ 
                          tp_in_seas+
                          tp_rr_seas,  
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

#results
colnames(cor.results) <- c('i', 'j', 'AIC')#Regression with ARMA(1,0) and ARMA (0,2) basically have the same aic so choose (1,0) 
cor.results %>% arrange(AIC)

#best fit


#run best fit model with arima (NOTE THAT EACH PREDICTOR BY ITSELF has the same corrolation structure (1,0,0))

Arima_fit2 <- Arima(log(sta2_int$tp_out_seas), xreg=x_vars, order=c(1,0,0))
summary(Arima_fit2)

#test coefficients
coeftest(Arima_fit2)

#test stationarity 
pacf(residuals(Arima_fit2))#looks good
acf(residuals(Arima_fit2))#looks good
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit2)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue


#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(log(tp_out_seas)  ~ 
                 tp_in_seas+
                 tp_rr_seas,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)


summary(ARMA_fit)
rsquared(ARMA_fit)
plot(ARMA_fit)


#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#llooks good
acf(residuals(ARMA_fit, type="normalized"))#looks good
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok

#ALTERNATIVE MODELS
#RR ONLY
ARMA_fit <-gls(log(tp_out_seas)  ~ 
                 tp_rr_seas,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)


summary(ARMA_fit)
rsquared(ARMA_fit)
plot(ARMA_fit)

#TP IN  ONLY
ARMA_fit <-gls(log(tp_out_seas)  ~ 
                 tp_in_seas,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)


summary(ARMA_fit)
rsquared(ARMA_fit)
plot(ARMA_fit)


#final model to pass on to piesewiseSEM should probably be one or the other

TP_out_c <-gls(log(tp_out_seas)  ~ 
                 tp_in_seas+
                 tp_rr_seas,  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int)

#### Model "piece" predicting Phosphorus Retention Rate####

#GLS model without temporal correlation structure (note: with collinear predictors)

tp_RR <-gls(tp_rr_seas ~  
              tp_in_seas + 
              tca_rr_seas+ 
              tn_rr_seas+ 
              por2 + 
              in_water_seas+
              temp_mean_seas+
              HRT_seas, 
            data = sta2_int)

car::vif(tp_RR)# check variance inflation, looks ok
#ca_rr and n_rr may be an issues together...

summary(tp_RR)
rsquared(tp_RR)
plot(tp_RR)#ldoesn't look great non-linear?

#looking at univartiate models
tp_RR <-gls(tp_rr_seas ~  tp_in_seas , data = sta2_int)
tp_RR <-gls(tp_rr_seas ~  tca_rr_seas , data = sta2_int)
tp_RR <-gls(tp_rr_seas ~  tn_rr_seas , data = sta2_int)
tp_RR <-gls(tp_rr_seas ~  in_water_seas , data = sta2_int)#this var is the problem...
tp_RR <-gls(tp_rr_seas ~  por2 , data = sta2_int)
tp_RR <-gls(tp_rr_seas ~  HRT_seas , data = sta2_int)
summary(tp_RR)
rsquared(tp_RR)
plot(tp_RR)

#test stationarity 
pacf(residuals(tp_RR))
acf(residuals(tp_RR))
Box.test(residuals(tp_RR), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_RR)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_RR), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$tp_in_seas,sta2_int$tca_rr_seas, sta2_int$tn_rr_seas, sta2_int$in_water_seas, sta2_int$por2, sta2_int$temp_mean_seas, sta2_int$HRT_seas)#create data frame with predictor variables
colnames(x_vars)<-c("TPC_IN","CA_RR","TN_RR", "IN_H2o", "por","temp", "HRT" )#give them names

cor(x_vars)#correlation values for vars
pairs(x_vars)#pairwise plots for vars

#run auto.arima
auto.arima(log1p(ts(sta2_int$tp_rr_int, frequency = 12)), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(sta2_int$tp_rr_seas, xreg=x_vars , trace=T) # Best model: Regression with ARIMA(2,0,0)



#without season--this seems fine...
Arima_fit2 <- Arima(sta2_int$tp_rr_seas, xreg=x_vars, order=c(2,0,0))
summary(Arima_fit2)


#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#looks good
acf(residuals(Arima_fit2))##looks good
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue





cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log1p(tp_rr_int)  ~ 
                          in_tp_c_int+
                          ca_rr_int+
                          tn_rr_int+ 
                          in_water_l_int+
                          por2+
                          temp_mean_int+
                          log1p(HRT_int),
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int, 
                        method="ML")
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(tp_rr_seas ~  
                 tp_in_seas + 
                 tca_rr_seas+ 
                 tn_rr_seas+ 
                 por2 + 
                 in_water_seas+
                 temp_mean_seas+
                 HRT_seas, 
               correlation = corARMA(p = 2, q = 0),
               data = sta2_int, 
               method="ML")

AIC(ARMA_fit)

#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)
round((1-pnorm(abs(ARMA_fit$coef)/sqrt(diag(ARMA_fit$varBeta))))*2, digits=8)#hand calculate pvalues


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
hist(E1)



#final model to pass on to piecewiseSEM

TPC_RR <-gls(tp_rr_seas ~  
               tp_in_seas + 
               tca_rr_seas+ 
               tn_rr_seas+ 
               por2 + 
               in_water_seas+
               temp_mean_seas+
               HRT_seas, 
             correlation = corARMA(p = 2, q = 0),
             data = sta2_int, 
             method="ML")

#### Model "piece" predicting Nitrogen Retention Rate####

#GLS model without temporal correlation structure 

tn_RR <-gls(tn_rr_seas~  
              tn_in_seas + 
              tca_rr_seas+ 
              por2 + 
              in_water_seas+
              temp_mean_seas+
              HRT_seas, 
            data = sta2_int)

car::vif(tn_RR)# check variance inflation, looks good


summary(tn_RR)
rsquared(tn_RR)
plot(tn_RR)#look at residual plot, looks ok


#looking at univartiate models
tn_RR <-gls(tn_rr_seas ~  tn_in_seas, data = sta2_int)
tn_RR <-gls(tn_rr_seas ~  tca_rr_seas , data = sta2_int)
tn_RR <-gls(tn_rr_seas ~  HRT_seas , data = sta2_int)
tn_RR <-gls(tn_rr_seas  ~  in_water_seas , data = sta2_int)
tn_RR <-gls(tn_rr_seas ~  por2 , data = sta2_int)


#test stationarity 
pacf(residuals(tn_RR))#lag of 1
acf(residuals(tn_RR))
Box.test(residuals(tn_RR), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_RR)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_RR), null="Trend") #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$tn_in_seas,sta2_int$tca_rr_seas, sta2_int$in_water_seas, sta2_int$por2, sta2_int$temp_mean_seas, sta2_int$HRT_seas)#create data frame with predictor variables
colnames(x_vars)<-c("TnC_IN","CA_RR", "IN_H2o", "por","temp","hrt" )#give them names

cor(x_vars)#correlation values for vars
pairs(x_vars)#pairwise plots for vairs

#run auto.arima
auto.arima(ts(sta2_int$tn_rr_seas, frequency = 12), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(sta2_int$tn_rr_seas, xreg=x_vars ) # Best model: Regression with ARIMA(0,0,1)




#without season--this seems fine...
Arima_fit2 <- Arima(sta2_int$tn_rr_seas, xreg=x_vars, order=c(0,0,1))
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




cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(tn_rr_seas~  
                          tn_in_seas + 
                          tca_rr_seas+ 
                          por2 + 
                          in_water_seas+
                          temp_mean_seas+
                          HRT_seas,
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int, 
                        method="ML")
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC) #p=1, q=0 best over (0,1) so use it
#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(tn_rr_seas~  
                 tn_in_seas + 
                 tca_rr_seas+ 
                 por2 + 
                 in_water_seas+
                 temp_mean_seas+
                 HRT_seas, 
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int, 
               method="ML")


#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)
round((1-pnorm(abs(ARMA_fit$coef)/sqrt(diag(ARMA_fit$varBeta))))*2, digits=8)#hand calculate pvalues


#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#looks good
acf(residuals(ARMA_fit, type="normalized"))#ooks good
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ehhhhh
hist(E1)



#final model to pass on to piecewiseSEM

TnC_RR <-gls(tn_rr_seas~  
               tn_in_seas + 
               tca_rr_seas+ 
               por2 + 
               in_water_seas+
               temp_mean_seas+
               HRT_seas, 
             correlation = corARMA(p = 1, q = 0),
             data = sta2_int, 
             method="ML")

#### Model "piece" predicting Calcium Retention Rate####

####GLS model without temporal correlation structure 

tca_RR <-gls(tca_rr_seas~  
              tca_in_seas + 
               por2 + 
               in_water_seas+
               temp_mean_seas+
               HRT_seas, 
             data = sta2_int)

AIC(tca_RR)
car::vif(tca_RR)# check variance inflation, looks ok


summary(tca_RR)
rsquared(tca_RR)
AIC(tca_RR)
plot(tca_RR)#look at residual plot, looks ok


#looking at univartiate models
tca_RR <-gls(tca_rr_seas ~  tca_in_seas, data = sta2_int)
tca_RR<-gls(tca_rr_seas ~  HRT_seas , data = sta2_int)
tca_RR <-gls(tca_rr_seas  ~  in_water_seas , data = sta2_int)
tca_RR <-gls(tca_rr_seas ~  por2 , data = sta2_int)
tca_RR <-gls(tca_rr_seas ~  temp_mean_seas , data = sta2_int)


#test stationarity 
pacf(residuals(tca_RR))#lag of 1
acf(residuals(tca_RR))
Box.test(residuals(tp_RR), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(tp_RR)) #want pvalue <0.05
tseries::kpss.test(residuals(tp_RR)) #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind(sta2_int$tca_in_seas, sta2_int$in_water_seas, sta2_int$por2, sta2_int$temp_mean_seas, sta2_int$HRT_seas)#create data frame with predictor variables
colnames(x_vars)<-c("Tca_IN", "IN_H2o", "por","temp","hrt" )#give them names

cor(x_vars)#correlation values for vars
pairs(x_vars)#pirwose plots for vairs

#run auto.arima
auto.arima(ts(sta2_int$ca_rr_int, frequency = 12), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(sta2_int$tca_rr_seas, xreg=x_vars , trace=T,seasonal = F) # Best model: Regression with ARIMA(2,0,1), but model with


#without season--this seems fine...
Arima_fit2 <- Arima(sta2_int$ca_rr_int, xreg=x_vars, order=c(2,0,1))#model with no ma or ar estimates
summary(Arima_fit2)

AIC(Arima_fit2)

#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#good
acf(residuals(Arima_fit2))#good
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue




cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(tca_rr_seas~  
                          tca_in_seas + 
                          por2 + 
                          in_water_seas+
                          temp_mean_seas+
                          HRT_seas,  
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)#aic same for model with no temporal correlation as best model here

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(tca_rr_seas~  
                 tca_in_seas + 
                 por2 + 
                 in_water_seas+
                 temp_mean_seas+
                 HRT_seas,  
               #correlation = corARMA(p = 0, q = 1),# this does not increase fit so no arma structure
               data = sta2_int,
               method="ML")



#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)
round((1-pnorm(abs(ARMA_fit$coef)/sqrt(diag(ARMA_fit$varBeta))))*2, digits=8)#hand calculate pvalues


#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#good
acf(residuals(ARMA_fit, type="normalized"))#good
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok
hist(E1)



#final model to pass on to piecewiseSEM
#residuals are heterosjedastic and non-normal...

TC_RR <-gls(tca_rr_seas~  
              tca_in_seas + 
              por2 + 
              in_water_seas+
              temp_mean_seas+
              HRT_seas,  
            #correlation = corARMA(p = 0, q = 1),# this does not increase fit so no arma structure
            data = sta2_int,
            method="ML")


#### Model "piece" predicting Hydraulic Retention Rate####


####GLS model without temporal correlation structure 

HRT <-gls(HRT_seas~  
            por2 + 
            in_water_seas+
            rain_mean_seas,
          data = sta2_int)


car::vif(HRT)# check variance inflation, looks ok


summary(HRT)
rsquared(HRT)
AIC(HRT)
plot(HRT)#look at residual plot, looks ok


#looking at univartiate models
HRT <-gls(log1p(HRT_int) ~  por2, data = sta2_int)
HRT <-gls(log1p(HRT_int)  ~  log(in_water_l_int) , data = sta2_int)
HRT <-gls(log1p(HRT_int) ~  log(rainfall_mean_int) , data = sta2_int)


#test stationarity 
pacf(residuals(HRT))#lag of 1 and 2
acf(residuals(HRT))
Box.test(residuals(HRT), lag=10, type="Ljung-Box") #want pvalue >0.05
tseries::adf.test(residuals(HRT)) #want pvalue <0.05
tseries::kpss.test(residuals(HRT)) #want pvalue >0.05

#Use auto.arima function to get estimates for p, i, and q
x_vars<-cbind( sta2_int$in_water_seas, sta2_int$por2, sta2_int$rain_mean_seas)#create data frame with predictor variables
colnames(x_vars)<-c( "IN_H2o", "por","precip" )#give them names

cor(x_vars)#correlation values for vars
pairs(x_vars)#pirwose plots for vairs

#run auto.arima
auto.arima(ts(sta2_int$ca_rr_int, frequency = 12), xreg=x_vars , trace=T) # Best model: Regression with ARIMA(1,0,0)(1,0,0)[12] errors 
auto.arima(sta2_int$HRT_seas, xreg=x_vars , trace=T, seasonal = F) # Best model: Regression with ARIMA(0,0,1)




#without season--this seems fine...
Arima_fit2 <- Arima(sta2_int$HRT_seas, xreg=x_vars, order=c(0,0,1))
summary(Arima_fit2)



#test coefficients
coeftest(Arima_fit2)
(1-pnorm(abs(Arima_fit2$coef)/sqrt(diag(Arima_fit2$var.coef))))*2 #hand calculate pvalues

#test stationarity 
pacf(residuals(Arima_fit2))#fine
acf(residuals(Arima_fit2))#fine
Box.test(residuals(Arima_fit2), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(Arima_fit)) #want low pvalue
tseries::kpss.test(residuals(Arima_fit2), null="Trend") #want high pvalue




cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(HRT_seas~  
                          por2 + 
                          in_water_seas+
                          rain_mean_seas,
                        correlation = corARMA(p = i, q = j),
                        data = sta2_int)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)# (0,1) .

#now model with gls because we can use it for piecewise SEM
ARMA_fit <-gls(HRT_seas~  
                     por2 + 
                     in_water_seas+
                     rain_mean_seas,
               correlation = corARMA(p = 0, q = 1),
               data = sta2_int)



#check out model
car::vif(ARMA_fit)
summary(ARMA_fit)
rsquared(ARMA_fit)

#test coefficients
coeftest(ARMA_fit)
round((1-pnorm(abs(ARMA_fit$coef)/sqrt(diag(ARMA_fit$varBeta))))*2, digits=8)#hand calculate pvalues


#test stationarity 
pacf(residuals(ARMA_fit, type="normalized"))#good
acf(residuals(ARMA_fit, type="normalized"))#good
Box.test(residuals(ARMA_fit, type="normalized"), lag=10, type="Ljung-Box")
tseries::adf.test(residuals(ARMA_fit, type="normalized")) #want low pvalue
tseries::kpss.test(residuals(ARMA_fit, type="normalized"), null="Trend") #want high pvalue



#plot resdiuals
E1<-residuals(ARMA_fit, type = "normalized")
plot(x=ARMA_fit,y=E1) #plot residuals - looks ok
qqnorm(E1)#qqplot
qqline(E1) # looks ok
hist(E1)



#final model to pass on to piecewiseSEM
#residuals are heterosjedastic and non-normal...

HRT_mod <-gls(HRT_seas~  
                   por2 + 
                   in_water_seas+
                   rain_mean_seas,
              correlation = corARMA(p = 1, q = 0),
              data = sta2_int)

####SEM####

#models: selected using aic for p and q fro script STA_2_models.R####

TP_outflow_mod <-gls(log1p(tp_out_seas)  ~ 
                        tp_in_seas+
                        tp_rr_seas,  
                        correlation = corARMA(p = 1, q = 0),
                        data = sta2_int,
                        method="ML")

TN_RR_mod <-gls(tn_rr_seas~  
                  tn_in_seas + 
                  tca_rr_seas+ 
                  por2 + 
                  in_water_seas+
                  temp_mean_seas+
                  HRT_seas, 
                correlation = corARMA(p = 1, q = 0),
                data = sta2_int, 
                method="ML")

TP_RR_mod <-gls(tp_rr_seas ~  
                 tp_in_seas + 
                 tca_rr_seas+ 
                 tn_rr_seas+ 
                 por2 + 
                 in_water_seas+
                 temp_mean_seas+
                 HRT_seas, 
               correlation = corARMA(p = 2, q = 0),
               data = sta2_int, 
               method="ML")
summary(TCA_RR_mod)
HRT_mod <-gls(HRT_seas~  
                por2 + 
                in_water_seas+
                rain_mean_seas,
              correlation = corARMA(p = 1, q = 0),
              data = sta2_int,
              method="ML")



TCA_RR_mod <-gls(tca_rr_seas~  
                    tca_in_seas + 
                    por2 + 
                    in_water_seas+
                    temp_mean_seas+
                    HRT_seas,
                    data = sta2_int,
                    method="ML")



####Path for MPD as response#### 
model1<-psem(TN_RR_mod,TCA_RR_mod,HRT_mod,TP_RR_mod,TP_outflow_mod)
summary(model1, .progressBar = F)
