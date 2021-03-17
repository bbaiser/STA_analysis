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

full_dat<-read.csv("data/4_sta.csv", row=1) #complete data frame compiled by Jing HU on 3/2/2021

head(full_dat)

#build model "piece" predicting total p outfloc conc. (out_tp_c )####

# out_tp_c =total p outfloc conc.
# tp_rr = total p retention rate
# in_tp_c =total p inflow conc.

#lme model

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
sep_sta<-ggplot(p_out_dat, aes(x=por, y=out_tp_c, group=sta)) +
  geom_line(aes(color=sta))

sep_sta

# all data combined time series
tog_sta<-ggplot(p_out_dat, aes(x=por, y=out_tp_c, group=1)) +
  geom_line()

tog_sta


tp_out <-lme(out_tp_c ~ tp_rr+ in_tp_c, random = ~ 1 | sta, data = p_out_dat, na.action = na.exclude)

summary(tp_out)

plot(p_out_dat$year,p_out_dat$out_tp_c)
r.squaredGLMM(tp_out)



r.squaredGLMM(tp_out)

??r2_nakagawa
E1<-resid(tp_out, type="pearson")
plot(x=na.omit(tp_out),y=na.omit(E1) )

