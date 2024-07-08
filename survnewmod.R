setwd("/home1/p309444/models/AIC/")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

survival1<-read.csv("survival_model.csv",stringsAsFactors = F)
survival1$SexEstimate<-as.factor(survival1$SexEstimate)
survival1$statbefore<-as.factor(survival1$statbefore)
survival1$occasionyear<-as.factor(survival1$occasionyear)
survival1$birthyear<-as.factor(survival1$birthyear)
survival1<-survival1%>%filter(age>0)

#Survival
survival_al<-gam(survival~s(age, k=15)+SexEstimate+lifespan+avg_bug+
                   s(occasionyear, bs="re")+s(birthyear, bs="re"),
                 method="REML",data=survival1, family=binomial(link = "logit"))

saveRDS(survival_al, "surv28mod.rds")

