setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

provisioning<-read.csv("provisioning_28_5.csv",stringsAsFactors = F, sep = ';', dec = ",")

provisioning$Observer<-as.factor(provisioning$Observer)
provisioning$BirdID<-as.factor(provisioning$BirdID)
provisioning$SexEstimate<-as.factor(provisioning$SexEstimate)
provisioning$newstat<-as.factor(provisioning$newstat)
provisioning$WatchType<-as.factor(provisioning$WatchType)

prov_al<-gam(prate_z~s(age,k=10)+newlifespan+SexEstimate+BroodSize+nr_helpers+
               newstat+avg_invert+s(birthyear, bs="re")+s(WatchType,bs="re")+
               s(BirdID, bs="re")+s(Observer, bs="re"), 
             method="REML",data=provisioning, family=gaussian)

saveRDS(prov_al, "prov28mod.rds")
