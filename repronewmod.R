setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

reprosuccess<-read.csv("repro_28_5.csv",stringsAsFactors = F)

reprosuccess$BirdID<-as.factor(reprosuccess$BirdID)
reprosuccess$sex<-as.factor(reprosuccess$sex)
reprosuccess$newstat<-as.factor(reprosuccess$newstat)

rs_al<-gam(ars~s(parent_age,k=15)+s(parent_age, by=sex, k=15)+sex+newlifespan+
             minrepro+maxrepro+flno+newstat+s(birthyear, bs="re")+s(BirdID, bs="re"),
           method="REML",data=reprosuccess, family=poisson)

saveRDS(rs_al, "rs28mod.rds")
