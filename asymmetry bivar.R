getwd()

setwd('c:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/')

physio<-read.csv("physio_28_5.csv",stringsAsFactors = F)
provisioning<-read.csv("provisioning_28_5.csv",stringsAsFactors = F, sep = ';',dec=',')

haematocrit<-read.csv("haematocrit_28_5.csv",stringsAsFactors = F)
telomeres<-read.csv("telomere_28_5.csv", stringsAsFactors = F)

library(tidyverse)
library(mgcv)
library(gratia)
install.packages('brms')
library(brms)
#gratia for checking gam diagnostics

# Code for bivariate GAMMs (see also: https://rdrr.io/cran/mgcv/man/mvn.html):
# b <- gam(list(y0~s(x0)+s(x1),y1~s(x2)+s(x3)),family=mvn(d=2),data=dat)
# b
# summary(b)
# plot(b,pages=1)
# solve(crossprod(b$family$data$R)) ## estimated cov matrix


#join data together

bmdata<-physio[,c("BirdID",'birthyear', "age_year", "BodyMass",'SexEstimate',
                  'newlifespan',"FieldPeriodID",'newstat',"Observer",'RightTarsus',
                  "avg_invert",'CatchTime_mins',"CatchID")]
names(bmdata)[names(bmdata)=='Observer']<-"bmobserver"

test<-bmdata%>%
  group_by(age_year,BirdID,FieldPeriodID)%>%
  mutate(bmcount=length(BodyMass))%>%
  mutate(newBodyMass=ifelse(bmcount>1, mean(BodyMass, na.rm=T),BodyMass))

test<-test[,-c(4,13,14)]
test<-unique(test)
bmdata<-test

names(provisioning)[names(provisioning)=='age_years']<-"age_year"
pdata<-provisioning[,c("BirdID","birthyear","prate","age_year","SexEstimate",
                       "newlifespan","FieldPeriodID",'newstat',"BroodSize",
                       'nr_helpers','avg_invert')]


bmdata <- bmdata[!is.na(bmdata$newBodyMass),]
bmdata<-bmdata[,-c(8)]
squished<-left_join(pdata, bmdata)
squished<-unique(squished)

squished$BodyMass_z<-scale(squished$newBodyMass)
squished$prate_z<-scale(squished$prate)
squished$RightTarsus_z<-scale(squished$RightTarsus)

squished$SexEstimate<-as.factor(squished$SexEstimate)
squished$newstat<-as.factor(squished$newstat)

bp<-gam(list(BodyMass_z~s(age_year, k=-1)+newlifespan+SexEstimate+newstat+
               avg_invert+RightTarsus_z+s(birthyear, bs='re')+s(BirdID, bs="re"),
         prate_z  ~ s(age_year,k=-1)+newlifespan+SexEstimate+newstat+BroodSize+
           nr_helpers+avg_invert+s(birthyear, bs='re')+s(BirdID, bs="re")),
        family=mvn(d=2),method="REML",data=squished)

summary(bp)
appraise(bp)

plot(bp, pages = 1)

solve(crossprod(bp$family$data$R)) ## estimated cov matrix

##############################
#telomere length and fat score -- mcmcglmm, linear

fatdata<-physio[,c("BirdID",'birthyear',"FatScore", "age_year", 'SexEstimate',
                  'newlifespan',"RightTarsus_z", "newstat",
                  "avg_invert",'Observer',"CatchTime_mins","CatchID")]
names(fatdata)[names(fatdata)=="Observer"]<-"fatobserver"
fatdata <- fatdata[!is.na(fatdata$FatScore),]

telodata<-telomeres[,c("BirdID",'age_year',"SexEstimate",'birthyear','avg_invert',
                       'newlifespan','newstat','Whodunnit','RTL',"CatchID","FieldPeriodID")]

telofat<-left_join(fatdata, telodata, by=c('BirdID', "age_year", 'SexEstimate',
                                           'CatchID','newlifespan','birthyear','newstat'))

telofat$RTL_z<-scale(telofat$RTL)

library(MCMCglmm)
telofat$SexEstimate<-as.factor(telofat$SexEstimate)

write.csv(telofat, "telofat.csv")

#example model-- needs tweaking + priors and other variables
#prior is wrong
prior1<-list(G = list(G1 = list(v=diag(2), nu=0.002),
                     G2 = list(V=diag(2), nu=0.002)),
            R= list(R1=list(v=diag(2), nu=0.002),
                    R2=list(v=diag(2), nu=0.002)))

tf<-MCMCglmm(cbind(FatScore, RTL_z)~age_year+newlifespan+SexEstimate+newstat+avg_invert,
         random = ~us(trait):BirdID + us(trait):birthyear, rcov=~us(trait):units,
         family = c('poisson', 'gaussian'), prior = prior1,
         data=telofat, nitt = 10000000, burnin=10000, thin=100)

#no of iterations- burnin divided by thin is 2000
#check parameter expanded prior


summary(tf)

#covar is -0.08


#I tried running this model with a gam bivariate and family=mvn(d=2)
#it turned out that the covariance was negative, which was unexpected since both gams came out with a linear negative slope
#possible that subset of data has changed the slope?
#when i plotted the model the slopes were wildly different than original models
#I then ran fat score separately in a gam with poisson and gaussian, the gaussian had a very different curve than poisson

telofat2<-filter(telofat, !is.na(telofat$RTL_z))

#simple test model
test2<-gam(list(FatScore~s(age_year)+newlifespan,
                RTL_z~s(age_year)+newlifespan), family = mvn(d=2), data=telofat2, method='REML')
plot(test2, pages = 1)
solve(crossprod(test2$family$data$R))

#covar is -0.02


#checking family and also testing whether data subset changed the slope
fat_poisson<-gam(FatScore~s(age_year)+newlifespan, family=poisson, method="REML", data=fatdata) ##same slope as original model
plot(fat_poisson)
fat_gaussian<-gam(FatScore~s(age_year)+newlifespan, family=gaussian, method="REML", data=fatdata)
plot(fat_gaussian)

telodata$RTL_z<-scale(telodata$RTL)
telo_tf<-gam(RTL_z~s(age_year)+newlifespan, family=gaussian, method="REML", data=telofat)  #positive slope
plot(telo_tf)
telo_simple<-gam(RTL_z~s(age_year)+newlifespan, family=gaussian, method="REML", data=telodata)  #negative slope
plot(telo_simple)

plot(telodata$age, telodata$RTL_z)


##############################################
#buffy coat female and telomere length

haemf<-filter(haematocrit, haematocrit$SexEstimate==0)
haemf<-haemf[,c('BirdID', "age","newlifespan",'newstat',"birthyear","CatchID",
                "avg_invert","Observer","wbc", "CatchTime",'FieldPeriodID')]
names(haemf)[names(haemf)=='age']<-'age_year'

telobuff<-left_join(haemf, telodata, by=c('BirdID', 'newlifespan','birthyear','age_year', "FieldPeriodID","CatchID"))
#both linear and gaussian, can do bivar gam

telobuff$RTL_z<-scale(telobuff$RTL)
telobuff$wbc_z<-scale(telobuff$wbc)

telobuff$newstat.x<-as.factor(telobuff$newstat.x)
telobuff2<-filter(telobuff, !is.na(telobuff$RTL))

telobuff2<-na.omit(telobuff2)

telobuff2$Whodunnit<-as.factor(telobuff2$Whodunnit)
telobuff2$Observer<-as.factor(telobuff2$Observer)

#can only compare in females
tb<-gam(list(RTL_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
               +s(Whodunnit, bs="re")+s(birthyear, bs='re')+s(BirdID, bs="re"),
         wbc_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
           s(Observer, bs='re')+s(birthyear, bs='re')+s(BirdID, bs="re")),
    family = mvn(d=2), data = telobuff2, method="REML")

solve(crossprod(tb$family$data$R))

0.0178214

appraise(tb)


############################
names(haemf)[names(haemf)=="age"]<-"age_year"
haemfat<- left_join(haemf, fatdata, by=c('BirdID',"age_year", 'newlifespan',
                                         'newstat','birthyear', 'CatchID'))

sum(is.na(haemfat$FatScore))






#the above are examples of doable models

#this model has no solution so far
malaria<-read.csv("malaria_28_5.csv", sep = ';', dec = ',')

mdata<-malaria[,c('BirdID', "age","newlifespan",'newstat',"birthyear","CatchID",
                "avg_invert","TestResult",'FieldPeriodID')]
haem_m<-haematocrit%>% filter(SexEstimate==1)
haem_m<-haem_m[,c('BirdID', "age","newlifespan",'newstat',"birthyear","CatchID",
                "avg_invert","Observer","wbc", "CatchTime",'FieldPeriodID')]


malhaem<-left_join(haem_m, mdata, by.x=c('BirdID', 'age','newlifespan','birthyear', "CatchID",'FieldPeriodID'))



test1234<-malhaem%>%
  group_by(age,BirdID, FieldPeriodID)%>%
  mutate(malcount=length(TestResult))%>%
  mutate(haemcount=length(wbc))%>%
  mutate(newwbc=ifelse(haemcount>1, mean(wbc, na.rm=T),wbc))


test1234<-test1234[,-c(6,8,10)]
test1234<-unique(test1234)

test1234$wbc_z<-scale(test1234$newwbc)

test1234<-test1234%>%filter(!is.na(TestResult))
#not linear nor quadratic
#different families

#possible options:
malhaem$Observer


mh1<-gam(list(TestResult~s(age, k=16)+newlifespan+avg_invert+newstat+s(BirdID, bs="re"),
              wbc_z~s(age, k=16)+newlifespan+avg_invert+newstat+s(BirdID, bs="re")),
         family=mvn(d=2), method="REML", data=test1234)

plot(mh1, pages = 1)
solve(crossprod(mh1$family$data$R))
0.04137108

resids<-simulateResiduals(mh1)
plotResiduals(resids)

appraise(mh1)
#possibly fine, only thing is malaria test result is binary 1,0
obj1<-compare_smooths(mal28mod, buffy28mod, smooths = 's(age)', partial_match=T)


obj2<-compare_smooths(body_28mod, prov28mod, smooths = c(1, 1))

draw(obj1)
draw(obj2)
View(obj1)

test1234<-as.data.frame(test1234)
mh2<-MCMCglmm(cbind(TestResult, wbc_z)~age+I(age^2)+newlifespan,
             random = ~us(trait):BirdID, rcov=~us(trait):units,
             family = c('categorical', 'gaussian'),data=test1234, nitt = 100000, burnin=1000, thin=50)


summary(mh2)
-0.06443
#i dont think its this tho

#different slope if gaussian vs poisson
#mvn2 probably doesnt satisfy the assumptions, so fat has a positive slope now
#mcmcglmm- must use square term
#or fine if linear?
#maybe i can use a mix of both..



