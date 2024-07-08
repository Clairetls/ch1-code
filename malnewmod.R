setwd("/home1/p309444/models/AIC")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

malaria<-read.csv("malaria_28_5.csv",stringsAsFactors = F, sep=";", dec=',')
malaria$BirdID<-as.factor(malaria$BirdID)
malaria$SexEstimate<-as.factor(malaria$SexEstimate)
malaria$newstat<-as.factor(malaria$newstat)

mal_al<-gam(TestResult~s(age,k=8)+SexEstimate+s(age,by = SexEstimate, k=8)+newlifespan+
              newstat+avg_invert+s(birthyear, bs="re")+s(BirdID, bs="re"),
            method="REML",data=malaria, family=binomial)

saveRDS(mal_al, "mal28mod.rds")
