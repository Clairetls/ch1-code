#how to cross validate gams

#take 90% of data, see how well model fits the rest of the 10% 
#use predict 
#if predict badly then yes 
#
getwd()
setwd("C:/PhD/Data")
library(tidyverse)
library(mgcv)


phys<-read.csv("physio_model.csv", stringsAsFactors = F)

# s(age_year,k=16)+lifespan+ RightTarsus_z+newstat+
#   TQ+SexEstimate+CatchTime_mins+avg_invert+
#   s(birthyear, bs="re")+s(Observer, bs="re")+s(BirdID, bs="re"), 
# data=physio, method = "REML", family = gaussian)

bm<-phys[,c("BirdID","BodyMass_z",'age_year', 'CatchTime_mins',"lifespan",'newstat','RightTarsus_z','SexEstimate','TQ','birthyear',"avg_invert","Observer")]
bm<-filter(bm, !is.na(bm$BodyMass_z))
bm_90<-bm[sample(1:nrow(bm),3543, replace=FALSE),]
bm_10<-anti_join(bm,bm_90)

# sum(bm_90 %in% bm_10==T)

bm_90$Observer<-as.factor(bm_90$Observer)
bm_90$SexEstimate<-as.factor(bm_90$SexEstimate)
bm_90$BirdID<-as.factor(bm_90$BirdID)

ogmodel_90<-gam(BodyMass_z~s(age_year,k=16)+lifespan+ RightTarsus_z+newstat+
                   TQ+SexEstimate+CatchTime_mins+avg_invert+
                   s(birthyear, bs="re")+s(Observer, bs="re")+s(BirdID, bs="re"), 
                 data=bm_90, method = "REML", family = gaussian)

######### Raph ###########

set.seed(42)

bm%>%
  mutate(bin = sample(1:5, n(), replace = T))%>%
  group_by(bin) %>%
  nest() %>%
  mutate(train = map(bin, function(bin) { bm %>% filter(bin != bin) }))

bm2<-bm%>%
  filter(!is.na(lifespan))

#cross validation- 
#fit model over percentage of data- say 90 or 80% 
#predict over the remaining 20 or 10% 
#compare the error distributions for the two 
#the better the fit of the model the more similar the error distributions of the residuals are between fitted and predict
#if over or underfitted, there should be huge difference. 

bm %>%
  drop_na(lifespan) %>%
  mutate(bin = sample(1:5, n(), replace = T)) %>%
  group_by(bin) %>%
  nest(bin) %>%
  mutate(train = map(bin, function(bin) { bm %>% filter(bin != bin) })) %>% #ormap(bin, ~ bm %>% filter(!=.x))? 
  mutate(res = map2(data, train, function(test, train) {
    
    fit <- gam(BodyMass_z~s(age_year,k=16)+lifespan, 
        data=train, method = "REML", family = gaussian)
    
    predict.gam(fit, test)
    
  }))


fit <- gam(BodyMass_z~s(age_year,k=16)+lifespan, 
           data=bm %>% filter(bin != 1), method = "REML", family = gaussian)

predict.gam(fit, bm %>% filter(bin == 1), exclude = s(BirdID, bs="re"))

##########################

gam(BodyMass_z~s(age_year,k=16)+lifespan, 
    data=bm_90, method = "REML", family = gaussian)

bm_90

predbm10<-predict.gam(ogmodel_90, bm_10, exclude = s(BirdID, bs="re"))

###################################
library(gamclass)
check<-CVgam(BodyMass_z~s(age_year,k=16)+lifespan, bm2, nfold = 10, printit = T)
plot(check$fitted)
check2<-CVgam(BodyMass_z~s(age_year,k=8)+lifespan, bm2, nfold = 10, printit = T)
plot(check2$fitted)

check3<-CVgam(BodyMass_z~s(age_year,k=5)+lifespan, bm2, nfold = 10, printit = T)
plot(check3$fitted)

plot(bm$BodyMass_z)
test<-gam(BodyMass_z~s(age_year, k=2)+lifespan, bm, method="REML", family=gaussian)
plot(test, select=1,seWithMean = T,shade=TRUE )
ggplot(bm, aes(x=BodyMass_z))+geom_density()
