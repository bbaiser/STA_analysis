#Packages

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

d<-c(1,1,1,1,2,2,2,3,3,3)
e<-c(1,2,3,4,1,2,3,1,2,3)
f<-cbind(d,e)


#complete data frame compiled by Jing HU on 3/2/2021
master_dat<-read.csv("data/R_sta_month.csv")

colnames(master_dat)
unique(master_dat$sta)

#make a list of STAs that we will be analyzing
sta_red<-c("sta_1E", "sta_1W", "sta_2",  "sta_34")

sta_4 <- master_dat %>%
  filter(sta %in% sta_red) %>%
  filter(year > 2006)  #take years 2006 forward
  
sta_5 <- master_dat %>%
  filter(sta %in% sta_red) %>%
  filter(year==2006 & month >4)%>%
  bind_rows(sta_4)%>%
  arrange(.,X)



