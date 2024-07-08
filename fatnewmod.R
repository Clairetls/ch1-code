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

fat_al<-gam(FatScore~s(age_year,k=15)+newlifespan+SexEstimate+
              newstat+avg_invert+CatchTime_mins+s(birthyear, bs="re")+
              s(age_year, BirdID, bs="re")+s(Observer, bs="re"),
            data=physio, method="REML", family=poisson)

fat_al2<-gam(FatScore~s(age_year,k=15)+newlifespan+SexEstimate+
              newstat+avg_invert+CatchTime_mins+s(birthyear, bs="re")+
              s(BirdID, bs="re")+s(Observer, bs="re"),
            data=physio, method="REML", family=poisson)

none<-gam(FatScore~s(age_year, k=15)+newlifespan, data=physio, method="REML", family=poisson)

sum(!is.na(physio$FatScore))

plot(none, select=1)
saveRDS(fat_al, "fat28mod.rds")
