####Model selection for individual STAs (2006-2020)

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
  select(out_tp_c,in_tp_c,tp_rr, sta,por2)%>%
  filter(complete.cases(.))

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
p_out_dat<- full_dat %>%
  filter(out_tp_c < 0.2)  




#look time series for STA2
sep_sta_2<-ggplot(subset(p_out_dat,sta %in% c("sta_2")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))+
  xlab("Period of Record (Month)") + ylab("Total P output conc.")


sep_sta_2

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c, lag=20, type="Ljung-Box")

#suset sta2 for the model
sta2<-p_out_dat%>%
      filter(sta %in% c("sta_2")) 


#lme model without Correlation
tp_out <-lm(log(out_tp_c) ~  in_tp_c+tp_rr, data = sta2)
#simple regression
tp_out <-lm(log(out_tp_c) ~  tp_rr, data = sta2)
tp_out <-lm(log(out_tp_c) ~  in_tp_c, data = sta2)


#plot
ggplot(sta2,aes(tp_rr, log(out_tp_c))) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)

#check multicollinearity
car::vif(lm(log(out_tp_c) ~  in_tp_c+tp_rr, data = sta2))

#check summary 
summary(tp_out)
r.squaredLR(tp_out) 

#check residuals
plot(tp_out)

# extract model residuals
E1<-resid(tp_out, type="pearson")

         
#explore temporal autocorrelation
Box.test(resid(tp_out), lag=20, type="Ljung-Box")#test for autocorr
acf(E1)
pacf(E1)

#######lme model WITH Correlation P = 1
tp_out_ARMA <-gls(log(out_tp_c) ~  in_tp_c, data = sta2,correlation = corARMA(p = 0, q = 1,form =~ 1))
#simple regression
tp_out <-lm(log(out_tp_c) ~  tp_rr, data = sta2)
tp_out <-lm(log(out_tp_c) ~  in_tp_c, data = sta2)

#check multicollinearity
car::vif(lm(log(out_tp_c) ~  in_tp_c+tp_rr, data = sta2))

#check summary 
summary(tp_out_ARMA)
r.squaredLR(tp_out_ARMA) 

#check residuals
plot(tp_out_ARMA)

# extract model residuals
E1<-resid(tp_out_ARMA, type="pearson")


#explore temporal autocorrelation
Box.test(resid(tp_out_ARMA), lag=20, type="Ljung-Box")#test for autocorr
acf(E1)
pacf(E1)


cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      tp_out_ARMA <-gls(log(out_tp_c) ~  
                         in_tp_c+
                         tp_rr, 
                        correlation = corARMA(p = i, q = j),
                        data = p_out_dat, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}
require(forecast)    

x <- as.matrix(sta2[,c("tp_rr","in_tp_c")])
fit3 <- auto.arima(log(sta2$out_tp_c), xreg=x)
auto.arima(sta2$out_tp_c)
summary(fit3)

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)
