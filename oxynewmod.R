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

oxy_al<-gam(sum_oxy_z~s(age_year,k=10)+newlifespan+SexEstimate+
              newstat+avg_invert+CatchTime_mins+s(birthyear, bs="re")+
              s(age_year,BirdID, bs="re"),data=oxy,method="REML",family = gaussian)


saveRDS(oxy_al, "oxy28mod.rds")