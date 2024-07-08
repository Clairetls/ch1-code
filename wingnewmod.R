setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

physio<-read.csv("physio_28_5.csv",stringsAsFactors = F)


physio$Observer<-as.factor(physio$Observer)
physio$BirdID<-as.factor(physio$BirdID)
physio$SexEstimate<-as.factor(physio$SexEstimate)
physio$newstat<-as.factor(physio$newstat)

winglength<-gam(WingLength_z~s(age_year,k=16)+newlifespan+SexEstimate+
                  RightTarsus_z+newstat+avg_invert+
                  s(birthyear, bs="re")+s(BirdID, bs="re")+s(Observer, bs="re"),
                data=physio,method = "REML", family = gaussian)

saveRDS(winglength, "wing28mod.rds")
