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
sta_4 <- full_dat %>%
  filter(sta %in% sta_red) %>%   # remove sta 5,6 and 5/6
  filter(year > 2006)  #take years 2006 forward




tp_out <-lme(out_tp_c ~ tp_rr+ in_tp_c, random = ~ 1 | sta, data = full_dat, na.action = na.exclude)

summary(full_dat$out_tp_c)

hist(full_dat$out_tp_c,  breaks = 1000)
r.squaredGLMM(tp_out)


summary(tp_out)
r.squaredGLMM(tp_out)

??r2_nakagawa
E1<-resid(tp_out, type="pearson")
plot(x=na.omit(tp_out),y=na.omit(E1) )

