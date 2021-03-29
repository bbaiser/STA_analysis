####Model selection for 4 STA analysis (2006-2020)

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



##look at time series of p out

#plot of all 4 time series together
sep_sta<-ggplot(p_out_dat, aes(x=por2, y=out_tp_c, group=sta)) +
  geom_line(aes(color=sta))

#test for autocorrelation
acf(p_out_dat$out_tp_c)
pacf(p_out_dat$out_tp_c)
Box.test(p_out_dat$out_tp_c, lag=20, type="Ljung-Box")

#1E
sep_sta_1E<-ggplot(subset(p_out_dat,sta %in% c("sta_1E")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))

sep_sta_1E

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_1E"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_1E"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_1E"))$out_tp_c, lag=20, type="Ljung-Box")


#1W
sep_sta_1W<-ggplot(subset(p_out_dat,sta %in% c("sta_1W")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))

sep_sta_1W

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_1W"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_1W"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_1W"))$out_tp_c, lag=20, type="Ljung-Box")


#2
sep_sta_2<-ggplot(subset(p_out_dat,sta %in% c("sta_2")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))


sep_sta_2

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_2"))$out_tp_c, lag=20, type="Ljung-Box")

#3/4

sep_sta_3_4<-ggplot(subset(p_out_dat,sta %in% c("sta_34")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))


sep_sta_3_4

#test for autocorrelation
acf(subset(p_out_dat,sta %in% c("sta_34"))$out_tp_c)
pacf(subset(p_out_dat,sta %in% c("sta_34"))$out_tp_c)
Box.test(subset(p_out_dat,sta %in% c("sta_34"))$out_tp_c, lag=20, type="Ljung-Box")

# all data combined into one time series
tog_sta<-ggplot(p_out_dat, aes(x=por2, y=out_tp_c, group=1)) +
  geom_line()

#test for autocorrelation
acf(p_out_dat$out_tp_c)
pacf(p_out_dat$out_tp_c)
Box.test(p_out_dat$out_tp_c, lag=20, type="Ljung-Box")

#lme model with sta as a random effect - need to log transform to even remotely meet assumptions
tp_out <-lme(log(out_tp_c) ~ tp_rr+ in_tp_c, random = ~ 1 | sta, data = p_out_dat)


#test for autocorrelation
summary(tp_out)
r.squaredGLMM(tp_out) 
Box.test(subset(p_out_dat$, lag=20, type="Ljung-Box")


#explore model residuals
E1<-resid(tp_out, type="pearson")
plot(x=tp_out,y=E1) #plot residuals
qqnorm(E1)#qqplot
qqline(E1)#qqline

#explore temporal autocorrelation
Box.test(resid(tp_out), lag=20, type="Ljung-Box")#test for autocorr
acf(E1)
pacf(E1)





#test p, q for corARMA ####

cor.results <- NULL
for(i in 0:3) {
  for(j in 0:3) {
    if(i>0 | j>0) {
      tp_out_ARMA <-lme(log(out_tp_c) ~ 
                          tp_rr + 
                          in_tp_c, 
                        random = ~ 1 | sta,
                        correlation = corARMA(form = ~ 1 | sta, p = i, q = j),
                        data = p_out_dat, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)

#  P Q   AIC
#1  3 2 447.9030
#2  1 2 451.6036
#3  2 2 452.9342
#4  3 0 452.9585
#5  1 3 453.5464
#6  3 1 453.6979
#7  3 3 454.9930
#8  2 3 455.0897
#9  1 0 460.2263
#10 2 1 461.0614
#11 1 1 461.5960
#12 2 0 461.8460
#13 0 3 478.5811
#14 0 2 508.1160
#15 0 1 539.7510


#gls model with ARMA
#gls model with sta as a random effect - need to log transform to even remotely meet assumptions
tp_out_ARMA <-gls(log(out_tp_c) ~ 
                    tp_rr + 
                    in_tp_c+ 
                    sta,
                  correlation = corARMA(p = 2, q=2 ),
                  data = p_out_dat)


summary(tp_out_ARMA)
r.squaredLR(tp_out_ARMA) 


#explore model residuals
E2<-resid(tp_out_ARMA, type="pearson")
plot(x=na.omit(tp_out_ARMA),y=na.omit(E2) ) #plot residuals
qqnorm(E2)#qqplot
qqline(E2)#qqline

#explore temporal autocorrelation

acf(E2,na.action = na.exclude)
pacf(E1,na.action = na.exclude)
Box.test(resid(tp_out_ARMA), lag=20, type="Ljung-Box")#test for autocorr

#lme model with ARMA
#lme model with sta as a random effect - need to log transform to even remotely meet assumptions
tp_out_ARMA <-lme(log(out_tp_c) ~ 
                    tp_rr + 
                    in_tp_c, 
                  random = ~ 1 | sta,
                  correlation = corARMA(form = ~ 1 | sta, p = 3, q=2 ),
                  data = p_out_dat)


summary(tp_out_ARMA)
r.squaredGLMM(tp_out_ARMA) 


#explore model residuals
E2<-resid(tp_out_ARMA, type="pearson")
plot(x=na.omit(tp_out_ARMA),y=na.omit(E2) ) #plot residuals
qqnorm(E2)#qqplot
qqline(E2)#qqline

#explore temporal autocorrelation

acf(E2,na.action = na.exclude)
pacf(E1,na.action = na.exclude)
Box.test(resid(tp_out_ARMA), lag=20, type="Ljung-Box")#test for autocorr


#(JH): test p, q for corARMA ####

cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      tp_out_ARMA <-lme(log(out_tp_c) ~ 
                          tp_rr + 
                          in_tp_c, 
                        random = ~ 1 | sta,
                        correlation = corARMA(form = ~ 1 | sta, p = i, q = j),
                        data = p_out_dat, 
                        na.action = na.exclude)
      cor.results<-as.data.frame(rbind(cor.results,c(i, j, AIC(tp_out_ARMA))))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results %>% arrange(AIC)

#JH: test p, q 
manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, p = 3, q = 0),
              sav_wsmodelTFOH)

AIC(manure)
#(JH): if p >3 or q >3, R reported fatal problem and stop the session     
