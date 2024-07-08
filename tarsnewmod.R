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

tars_al<-gam(RightTarsus_z~s(age_year,k=16)+newlifespan+SexEstimate+
               avg_invert+newstat+s(BirdID, bs="re")+
               s(birthyear, bs="re")+s(Observer, bs="re"), 
             data = physio, method="REML", family=gaussian)

saveRDS(tars_al, "tars28mod.rds")