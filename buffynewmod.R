setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

haematocrit<-read.csv("haematocrit_28_5.csv",stringsAsFactors = F)

haematocrit$Observer<-as.factor(haematocrit$Observer)
haematocrit$BirdID<-as.factor(haematocrit$BirdID)
haematocrit$SexEstimate<-as.factor(haematocrit$SexEstimate)
haematocrit$newstat<-as.factor(haematocrit$newstat)
haematocrit$CatchTime<-chron(times=haematocrit$CatchTime)
haematocrit$CatchTime_mins<-60 * 24 * as.numeric(times(haematocrit$CatchTime))

Buffy_al<-gam(wbc_z~s(age, by = SexEstimate, k=15)+SexEstimate+newlifespan+
                newstat+avg_invert+CatchTime_mins+
                s(birthyear, bs="re")+s(BirdID, bs="re")+s(Observer, bs="re"),
              method="REML",data=haematocrit, family=gaussian)
saveRDS(Buffy_al, "buffy28mod.rds")
