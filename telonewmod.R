setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

telomeres<-read.csv("telomere_28_5.csv", stringsAsFactors = F)

telomeres$Whodunnit<-as.factor(telomeres$Whodunnit)
telomeres$BirdID<-as.factor(telomeres$BirdID)
telomeres$SexEstimate<-as.factor(telomeres$SexEstimate)
telomeres$newstat<-as.factor(telomeres$newstat)

telo_al<-gam(RTL_z~s(age_year, k=15)+SexEstimate+s(age_year,by = SexEstimate,k=15)+
               newlifespan+newstat+avg_invert+s(birthyear, bs="re")
             +s(BirdID, bs="re")+s(Whodunnit, bs="re"), 
             method="REML",data=telomeres, family=gaussian)

saveRDS(telo_al, "telo28mod.rds")