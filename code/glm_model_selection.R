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
  filter(out_tp_c < 0.2)  #take years 2006 forward



##look at time series of p out

#plot time series for each sta
sep_sta<-ggplot(p_out_dat, aes(x=por2, y=out_tp_c, group=sta)) +
  geom_line(aes(color=sta))

sep_sta

#1E
sep_sta_1E<-ggplot(subset(p_out_dat,sta %in% c("sta_1E")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))

sep_sta_1E

#1W
sep_sta_1W<-ggplot(subset(p_out_dat,sta %in% c("sta_1W")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))

sep_sta_1W

#1W
sep_sta_1W<-ggplot(subset(p_out_dat,sta %in% c("sta_1W")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))

sep_sta_1W


#2
sep_sta_2<-ggplot(subset(p_out_dat,sta %in% c("sta_2")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))


sep_sta_2

#3/4

sep_sta_3_4<-ggplot(subset(p_out_dat,sta %in% c("sta_34")), aes(x=por2, y=out_tp_c)) +
  geom_line(aes(color=sta))


sep_sta_3_4

# all data combined time series
tog_sta<-ggplot(p_out_dat, aes(x=por2, y=out_tp_c, group=1)) +
  geom_line()

tog_sta


#lme model with sta as a random effect - need to log transform to even remotely meet assumptions
tp_out <-lme(log(out_tp_c) ~ tp_rr+ in_tp_c, random = ~ 1 | sta, data = p_out_dat)

summary(tp_out)
r.squaredGLMM(tp_out) 



#explore model residuals
E1<-resid(tp_out, type="pearson")
plot(x=na.omit(tp_out),y=na.omit(E1) ) #plot residuals
qqnorm(E1)#qqplot
qqline(E1)#qqline

#explore temporal autocorrelation
Box.test(resid(tp_out), lag=20, type="Ljung-Box")#test for autocorr
acf(E1,na.action = na.exclude)
pacf(E1,na.action = na.exclude)





#test p, q for corARMA ####

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


#lme model with ARMA
#lme model with sta as a random effect - need to log transform to even remotely meet assumptions
tp_out_ARMA <-lme(out_tp_c ~ 
                    tp_rr + 
                    in_tp_c, 
                  random = ~ 1 | sta,
                  correlation = corARMA(form = ~ 1 | sta, p = 1, q=2 ),
                  data = p_out_dat)

zz<-diff(p_out_dat$out_tp_c,lag=4)
Box.test(zz, lag=20, type="Ljung-Box")#test for autocorr
acf(p_out_dat$out_tp_c)

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
