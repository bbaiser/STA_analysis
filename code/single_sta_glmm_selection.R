####Model selection for individual STAs (2006-2020)

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
  geom_line(aes(color=sta))


sep_sta_2

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c, lag=20, type="Ljung-Box")

#suset sta2 for the model
sta2<-p_out_dat%>%
      filter(sta %in% c("sta_2")) 


#lme model without Correlation
tp_out <-gls(out_tp_c ~ tp_rr+ in_tp_c, data = sta2)
sta2$out_tp_c

#test for autocorrelation
summary(tp_out)
r.squaredLR(tp_out) 

         
         
#explore model residuals
E1<-resid(tp_out, type="pearson")
plot(x=tp_out,y=E1) #plot residuals
qqnorm(E1)#qqplot
qqline(E1)#qqline
         
#explore temporal autocorrelation
Box.test(resid(tp_out), lag=20, type="Ljung-Box")#test for autocorr
acf(E1)
         pacf(E1)