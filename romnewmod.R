setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

oxy<-read.csv("oxy_28_5.csv", stringsAsFactors = F)

oxy$BirdID<-as.factor(oxy$BirdID)
oxy$SexEstimate<-as.factor(oxy$SexEstimate)
oxy$newstat<-as.factor(oxy$newstat)
oxy$CatchTime<-chron(times=oxy$CatchTime)
oxy$CatchTime_mins<-60 * 24 * as.numeric(times(oxy$CatchTime))


rom_al<-gam(sum_roms_z~s(age_year,k=14)+newlifespan+SexEstimate+
              newstat+avg_invert+CatchTime_mins+
              s(birthyear, bs="re")+s(BirdID, bs="re"), 
            method="REML",data=oxy, family=gaussian)

saveRDS(rom_al, "rom28mod.rds")