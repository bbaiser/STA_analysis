###Piecewise SEM for STA 2

# Original author: Ben Baiser
# Last updated: 12/15/2020

#######
#setwd
setwd("C:/Users/bbaiser/Dropbox/current research/CONTINENTAL PITCHER PLANT SURVEY/ANALYSIS")

# Load required libraries
library(nlme)
library(piecewiseSEM)
library(semPlot)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

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




#models: selected using aic for p and q fro script STA_2_models.R####

TP_outflow_mod <-gls(log(out_tp_c_int)  ~ 
                 in_tp_c_int+
                 log1p(tp_rr_int),  
               correlation = corARMA(p = 1, q = 0),
               data = sta2_int, 
               method="ML")

TN_RR_mod <-gls(tn_rr_int  ~ 
               in_tn_c_int+
               ca_rr_int+
               in_water_l_int+
               por2+
               temp_mean_int+
               log1p(HRT_int),  
             correlation = corARMA(p = 1, q = 0),
             data = sta2_int, 
             method="ML")

TP_RR_mod <-gls(log1p(tp_rr_int)  ~ 
               in_tp_c_int+
               ca_rr_int+
               tn_rr_int+ 
               in_water_l_int+
               por2+
               temp_mean_int+
               log1p(HRT_int),
             correlation = corARMA(p = 3, q = 2),
             data = sta2_int, 
             method="ML")


HRT_mod <-gls(log1p(HRT_int)~  
                por2 + 
                in_water_l_int+
                rainfall_mean_int,
              correlation = corARMA(p = 1, q = 0),
              data = sta2_int,
              method="ML")



TCA_RR_mod <-gls(ca_rr_int~  
              in_ca_c_int + 
              por2 + 
              in_water_l_int+
              temp_mean_int+
              log1p(HRT_int), 
            data = sta2_int,
            method="ML")



####Path for MPD as response#### 
model1<-psem(TN_RR_mod,TCA_RR_mod,HRT_mod,TP_RR_mod,TP_outflow_mod)
summary(model1, .progressBar = F)

#save out coefficents table
mod1_coefs<-coefs(model1)
write.csv(mod1_coefs, file = "results/mpd_model.csv", quote = FALSE, row.names = F)


#Direct Effects on microbes
microbes.table<-mod1_coefs[which(mod1_coefs$Response== "mpd.obs" & mod1_coefs$P.Value <=0.05), ]
microbes.table

#Direct Effects on midges
midge.table<-mod1_coefs[which(mod1_coefs$Response== "total_midge" & mod1_coefs$P.Value <=0.05), ]
midge.table

#Direct Effects on plant morphology
plant.table<-mod1_coefs[which(mod1_coefs$Response== "plant_PCA" & mod1_coefs$P.Value <=0.05), ]
plant.table


#Direct Effects on plant morphology
volume.table<-mod1_coefs[which(mod1_coefs$Response== "log1p(volume)" & mod1_coefs$P.Value <=0.05), ]
volume.table

####Path for MPDz as response#### 

#specify set of models for the path analysis (from best fit models in prelim_glm.R)
model2<-psem(plant,prey,volume,pH,midge,flies,mosquito,mpdz, total_midge %~~%flesh_flies,
             total_mosquito%~~%flesh_flies)
summary(model2, .progressBar = F)



#save out coefficents table
mod2_coefs<-coefs(model2)
write.csv(mod2_coefs, file = "results/mpdz.csv", quote = FALSE, row.names = F)



#Direct Effects on microbes
microbes.table<-mod2_coefs[which(mod2_coefs$Response== "mpd.obs.z" & mod2_coefs$P.Value <=0.05), ]
microbes.table

#Direct Effects on midges
midge.table<-mod2_coefs[which(mod2_coefs$Response== "total_midge" & mod2_coefs$P.Value <=0.05), ]
midge.table

#Direct Effects on plant morphology
plant.table<-mod2_coefs[which(mod2_coefs$Response== "plant_PCA" & mod2_coefs$P.Value <=0.05), ]
plant.table


#Direct Effects on plant morphology
volume.table<-mod2_coefs[which(mod2_coefs$Response== "log1p(volume)" & mod2_coefs$P.Value <=0.05), ]
volume.table


####Path for chao1 as response#### 

#specify set of models for the path analysis (from best fit models in prelim_glm.R)
model3<-psem(plant,prey,volume,pH,midge,flies,mosquito,chao,total_midge %~~%flesh_flies,
             total_mosquito%~~%flesh_flies)

summary(model3, .progressBar = F)

summary(lm(pitch$chao1~pitch$plant_PCA))

plot(lm(log(pitch$Chao_1_closed)~pitch$ph))

#save out coefficents table
mod3_coefs<-coefs(model3)
write.csv(mod3_coefs, file = "results/chao_open.csv", quote = FALSE, row.names = F)



#Direct Effects on microbes
microbes.table<-mod3_coefs[which(mod3_coefs$Response== "chao1" & mod3_coefs$P.Value <=0.05), ]
microbes.table

#Direct Effects on midges
midge.table<-mod3_coefs[which(mod3_coefs$Response== "total_midge" & mod3_coefs$P.Value <=0.05), ]
midge.table

#Direct Effects on plant morphology
plant.table<-mod3_coefs[which(mod3_coefs$Response== "plant_PCA" & mod3_coefs$P.Value <=0.05), ]
plant.table


#Direct Effects on plant morphology
volume.table<-mod3_coefs[which(mod3_coefs$Response== "log1p(volume)" & mod3_coefs$P.Value <=0.05), ]
volume.table