#Packages

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

#import data and clean data####

master_dat<-read.csv("data/R_sta_month.csv") #complete data frame compiled by Jing HU on 3/2/2021

head(master_dat)


#make a list of STAs that we will be analyzing 
sta_red<-c("sta_1E", "sta_1W", "sta_2",  "sta_34")

#remove stas and years we will not analyze
sta_4 <- master_dat %>%
  filter(sta %in% sta_red) %>%   # remove sta 5,6 and 5/6
  filter(year > 2006)  #take years 2006 forward

#remove the first 4 months of 2006 for three of the four sta's to standardize period of record  
sta_5 <- master_dat %>%
  filter(sta %in% sta_red) %>% # remove sta 5,6 and 5/6
  filter(year==2006 & month >4)%>% #now only take month 5-12 for 2006 from the remaining stas
  bind_rows(sta_4)%>% #combine the 2006 data with the 2007-2020 data (sta4) from above
  arrange(.,X)%>% #reorder
  mutate(por= paste(year, month))%>% #made a period of record column by combining year and month
  mutate(por2=rep(seq(1, 168),4))


#save reduced data frame out to data folder
write.csv(sta_5, "data/4_sta.csv")


