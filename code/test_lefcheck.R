# Load required libraries
library(dplyr)
library(GGally)
library(ggplot2)
library(gridExtra)
library(nlme)
# devtools::install_github("jslefche/piecewiseSEM@2.0")
library(piecewiseSEM) # version 2.0
library(plotrix)
library(readxl)
library(tidyr)
library(vegan)


#####################################################3

sav_wsmodelTFOH<-read.csv("data/sav_wsmodelTFOH.csv")
sav_wsmodelMH<-read.csv("data/sav_wsmodelMH.csv")

# Test for interaction
#lapply(c(sav_wsmodelTF, sav_wsmodelOH, sav_wsmodelMH, sav_wsmodelPH), function(i) {
#next line was rewritten by JH  
lapply(list(TFOH = sav_wsmodelTFOH, MH = sav_wsmodelMH, PH = sav_wsmodelPH), function(i) { 
  
  SAV <- lme(log10.Area_HA ~
               log10.flow +
               log10.nptn +
               log10.nptn:log10.flow +
               log10.ptn +
               log10.ptn:log10.flow +
               log10.tssx +
               log10.tssx:log10.flow +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             data = i)
  
  d <- data.frame(
    #next line was deleted by JH
    #zone = unique(i$SalinityZone),
    model = c("All", "NP source", "P source", "TSS", "None"),
    AIC = c(
      AIC(SAV),
      AIC(update(SAV, . ~ . - log10.nptn:log10.flow)),
      AIC(update(SAV, . ~ . - log10.ptn:log10.flow)),
      AIC(update(SAV, . ~ . - log10.tssx:log10.flow)),
      AIC(update(SAV, . ~ . - log10.nptn:log10.flow - log10.ptn:log10.flow - log10.tssx:log10.flow))
    ))
  
  d$deltaAIC <- d$AIC - min(d$AIC)
  
  return(d)
  
} )


#(JH): test p, q for corARMA 

cor.results <- NULL
for(i in 0:2) {
  for(j in 0:2) {
    if(i>0 | j>0) {
      manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, p = i, q = j),
                    sav_wsmodelTFOH)
      cor.results<-rbind(cor.results,c(i, j, AIC(manure)))
    }
  }
}

colnames(cor.results) <- c('i', 'j', 'AIC')
cor.results

#JH: test p, q 
manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, p = 0, q = 2),
              sav_wsmodelTFOH)

AIC(manure)
#(JH): if p >3 or q >3, R reported fatal problem and stop the session                                       



############################################
# Create SEMs for each salinity zone with TN
TFOH.TN.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelTFOH),
  
  
  fertilizer <- lme(log10.fertilizerN ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelTFOH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptn ~ Developed + log10.manureN + log10.fertilizerN + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelTFOH),
  
  ptp <- lme(log10.ptn ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelTFOH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptn +
               log10.ptn +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelTFOH),
  
  sav_wsmodelTFOH
  
)

MH.TN.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelMH),
  
  
  fertilizer <- lme(log10.fertilizerN ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelMH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptn ~ Developed + log10.manureN + log10.fertilizerN + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelMH),
  
  ptp <- lme(log10.ptn ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelMH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptn +
               log10.ptn +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelMH),
  
  sav_wsmodelMH
  
)

PH.TN.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureN ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelPH),
  
  
  fertilizer <- lme(log10.fertilizerN ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelPH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptn ~ Developed + log10.manureN + log10.fertilizerN + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelPH),
  
  ptp <- lme(log10.ptn ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelPH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptn +
               log10.ptn +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelPH),
  
  sav_wsmodelPH
  
)

# Bind SEMs into list
sems.TN.list <- list(TFOH = TFOH.TN.sem, MH = MH.TN.sem, PH = PH.TN.sem)

# Check residuals
# lapply(sems.list, function(i) plot(i))

# Get summary output
lapply(sems.TN.list, summary)

lapply(sems.TN.list, coefs)

lapply(sems.TN.list, rsquared)

lapply(sems.TN.list, function(x) nobs(x[[1]]))
#JH: Calculate the number of observations in object

####################################

# Repeat with TP instead of TN
TFOH.TP.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureP ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelTFOH),
  
  
  fertilizer <- lme(log10.fertilizerP ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelTFOH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptp ~ Developed + log10.manureP + log10.fertilizerP + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelTFOH),
  
  ptp <- lme(log10.ptp ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelTFOH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptp +
               log10.ptp +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelTFOH),
  
  sav_wsmodelTFOH
  
)

MH.TP.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureP ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelMH),
  
  
  fertilizer <- lme(log10.fertilizerP ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelMH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptp ~ Developed + log10.manureP + log10.fertilizerP + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelMH),
  
  ptp <- lme(log10.ptp ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelMH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptp +
               log10.ptp +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelMH),
  
  sav_wsmodelMH
  
)

PH.TP.sem <- psem(
  
  # Predictors of manure and fertilizer
  manure <- lme(log10.manureP ~ Agro + log10.WatershedHa,
                random = ~ 1 | SUBEST_ID,
                correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                sav_wsmodelPH),
  
  
  fertilizer <- lme(log10.fertilizerP ~ Agro + log10.WatershedHa,
                    random = ~ 1 | SUBEST_ID,
                    correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
                    sav_wsmodelPH),
  
  # Predictors of nutrients
  nptp <- lme(log10.nptp ~ Developed + log10.manureP + log10.fertilizerP + log10.WatershedHa,
              random = ~ 1 | SUBEST_ID,
              correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
              sav_wsmodelPH),
  
  ptp <- lme(log10.ptp ~ Developed + log10.WatershedHa,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelPH),
  
  # Predictors of SAV
  SAV <- lme(log10.Area_HA ~
               log10.Area_HA1 +
               log10.flow *
               log10.nptp +
               log10.ptp +
               log10.tssx +
               log10.Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | SUBEST_ID,
             correlation = corARMA(form = ~ 1 | SUBEST_ID/Year, q = 2),
             sav_wsmodelPH),
  
  sav_wsmodelPH
  
)

# Bind SEMs into list
sems.TP.list <- list(TFOH = TFOH.TP.sem, MH = MH.TP.sem, PH = PH.TP.sem)

# Check residuals
# lapply(sems.list, function(i) plot(i))

# Get summary output
lapply(sems.TP.list, summary)

lapply(sems.TP.list, coefs)

lapply(sems.TP.list, rsquared)

lapply(sems.TP.list, function(x) nobs(x[[1]]))

### BAY-WIDE STRUCTURAL EQUATION MODEL #############################

# Create SEM
station.sem <- psem(
  
  # Predictors of chl-a
  chla <- lme(log10.chla ~
                tp +
                tn +
                log10.WTEMP,
              random = ~ 1 | STATION/Year,
              correlation = corARMA(form = ~ 1 | STATION/Year, q = 2),
              control = lmeControl(opt = "optim"),
              data = sav_env),
  
  # Predictors of secchi
  secchi <- lme(log10.SECCHI ~
                  log10.WTEMP +
                  log10.chla +
                  tn +
                  tp,
                random = ~ 1 | STATION/Year,
                correlation = corARMA(form = ~ 1 | STATION/Year, q = 2),
                control = lmeControl(opt = "optim"),
                data = sav_env),
  
  Richness <- lme(Richness ~
                    log10.SAVCover1 +
                    log10.chla +
                    log10.SALINITY +
                    tp +
                    log10.SECCHI +
                    log10.Hectares1MPlusSAVComp,
                  random = ~ 1 | STATION/Year,
                  correlation = corARMA(form = ~ 1 | STATION/Year, q = 2),
                  control = lmeControl(opt = "optim"),
                  data = sav_env),
  
  # Predictors of SAV
  SAV <- lme(log10.SAVCover ~
               log10.SAVCover1 +
               log10.chla +
               log10.SECCHI +
               log10.WTEMP +
               log10.SALINITY +
               tn +
               tp +
               Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | STATION/Year,
             correlation = corARMA(form = ~ 1 | STATION/Year, q = 2),
             control = lmeControl(opt = "optim"),
             data = sav_env),
  
  tn %~~% tp,
  
  log10.SALINITY %~~% log10.SECCHI,
  
  data = sav_env
  
)

# Get summary
summary(station.sem)

# Estimate indirect effects of nutrients
station.coefs <- coefs(station.sem)

Btp.chla <- station.coefs[1, 8] * # tp on chla
  station.coefs[15, 8] # chla on SAV

Btp.secchi <- station.coefs[6, 8] * # tn on secchi
  station.coefs[16, 8] # secchi on SAV

Btn.secchi <- station.coefs[7, 8] * # tp on secchi
  station.coefs[16, 8] # secchi on SAV

Btp.chla.secchi <- station.coefs[1, 8] * # tp on chla
  station.coefs[5, 8] * # chla on secchi
  station.coefs[15, 8] # secchi on SAV

(Bindirect <- Btp.chla + Btp.secchi + Btn.secchi + Btp.chla.secchi)

(Btn.sav <- station.coefs[19 , 8])

Btn.sav/Bindirect


# Re-run SEM but include TSS and remove any years prior to 1999
sav_env99 <- subset(sav_env, Year > 1999)

sav_env99 <- na.omit(sav_env99)

station99.sem <- psem(
  
  # Predictors of chl-a
  chla <- lme(log10.chla ~
                tp +
                tn +
                log10.WTEMP +
                log10.SALINITY,
              random = ~ 1 | STATION/Year,
              # correlation = corAR1(form = ~ 1 | STATION/Year),
              control = lmeControl(opt = "optim"),
              data = sav_env99),
  
  # Predictors of secchi
  secchi <- lme(log10.SECCHI ~
                  log10.WTEMP +
                  log10.chla +
                  tn +
                  tp,
                random = ~ 1 | STATION/Year,
                # correlation = corAR1(form = ~ 1 | STATION/Year),
                control = lmeControl(opt = "optim"),
                data = sav_env99),
  
  Richness <- lme(Richness ~
                    log10.chla +
                    log10.SALINITY +
                    tn +
                    log10.Hectares1MPlusSAVComp,
                  random = ~ 1 | STATION/Year,
                  # correlation = corAR1(form = ~ 1 | STATION/Year),
                  control = lmeControl(opt = "optim"),
                  data = sav_env99),
  
  # Predictors of SAV
  SAV <- lme(log10.SAVCover ~
               log10.SAVCover1 +
               log10.chla +
               log10.SECCHI +
               log10.WTEMP +
               TSS +
               log10.SALINITY +
               tn +
               tp +
               Richness +
               log10.Hectares1MPlusSAVComp,
             random = ~ 1 | STATION/Year,
             # correlation = corAR1(form = ~ 1 | STATION/Year),
             control = lmeControl(opt = "optim"),
             data = sav_env99),
  
  tn %~~% tp,
  
  log10.SECCHI %~~% TSS,
  
  # Remove odd significant tests from the basis set
  
  log10.SECCHI %~~% log10.SALINITY,
  
  log10.SECCHI %~~% log10.SAVCover1,
  
  log10.chla %~~% log10.SAVCover1,
  
  log10.chla %~~% log10.Hectares1MPlusSAVComp,
  
  log10.SECCHI %~~% log10.Hectares1MPlusSAVComp,
  
  data = sav_env99
  
)

summary(station99.sem)