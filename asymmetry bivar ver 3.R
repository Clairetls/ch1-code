getwd()

setwd('c:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/')
fat28mod<-readRDS("c:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/fat28mod.rds")
library(tidyverse)
library(mgcv)
library(lme4)
library(brms)
library(glmm)
library(lmerTest)
library(MCMCglmm)

# modelsummary(body_28mod)
###################
physio<-read.csv("physio_28_5.csv",stringsAsFactors = F)
provisioning<-read.csv("provisioning_28_5.csv",stringsAsFactors = F, sep = ';',dec=',')

haematocrit<-read.csv("haematocrit_28_5.csv",stringsAsFactors = F)
telomeres<-read.csv("telomere_28_5.csv", stringsAsFactors = F)



# Code for bivariate GAMMs (see also: https://rdrr.io/cran/mgcv/man/mvn.html):
# b <- gam(list(y0~s(x0)+s(x1),y1~s(x2)+s(x3)),family=mvn(d=2),data=dat)
# b
# summary(b)
# plot(b,pages=1)
# solve(crossprod(b$family$data$R)) ## estimated cov matrix


#join data together

bmdata<-physio[,c("BirdID",'birthyear', "age_year", "BodyMass",'SexEstimate',
                  'newlifespan','newstat',"Observer",'RightTarsus',
                  "avg_invert",'CatchTime_mins',"CatchID")]
names(bmdata)[names(bmdata)=='Observer']<-"bmobserver"

test<-bmdata%>%
  group_by(age_year,BirdID)%>%
  mutate(bmcount=length(BodyMass))%>%
  mutate(newBodyMass=ifelse(bmcount>1, mean(BodyMass, na.rm=T),BodyMass),
         newtars=ifelse(bmcount>1, mean(RightTarsus, na.rm=T),RightTarsus),
         bmobserver=sample(bmobserver,1),
         CatchTime_mins=sample(CatchTime_mins, 1))

test<-test%>% filter(!is.na(newBodyMass))
test<-test[,-c(4,9,12)]
test<-unique(test)
bmdata<-test

names(provisioning)[names(provisioning)=='age_years']<-"age_year"
pdata<-provisioning[,c("BirdID","birthyear","prate","age_year","SexEstimate",
                       "newlifespan",'newstat',"BroodSize",
                       'nr_helpers','avg_invert')]




# bmdata<-bmdata[,-c(8)]
squished<-left_join(pdata, bmdata, by=c('BirdID', 'age_year',
                                        'SexEstimate', 'birthyear', 'newlifespan'))
squished<-unique(squished)
bodyprov<-filter(squished, !is.na(squished$newBodyMass))

bodyprov$BodyMass_z<-scale(bodyprov$newBodyMass)
bodyprov$prate_z<-scale(bodyprov$prate)
bodyprov$RightTarsus_z<-scale(bodyprov$newtars)


postdeclinebp<-bodyprov%>%filter(age_year>4)

bodyprov$SexEstimate<-as.factor(bodyprov$SexEstimate)
bodyprov$newstat<-as.factor(bodyprov$newstat)
bodyprov$BirdID<-as.factor(bodyprov$BirdID)
bodyprov$birthyear<-as.factor(bodyprov$birthyear)
plot(bodyprov$age_year, bodyprov$BodyMass_z)

#whole dataset
bp<-gam(list(BodyMass_z~s(age_year, k=-1)+newlifespan+SexEstimate+newstat.y+
               avg_invert.y+RightTarsus_z+s(birthyear, bs='re')+s(age_year, BirdID, bs="re"),
         prate_z~ s(age_year, k=-1)+newlifespan+SexEstimate+newstat.x+BroodSize+
           nr_helpers+avg_invert.x+s(birthyear, bs='re')+s(age_year, BirdID, bs="re")),
        family=mvn(d=2),method="REML",data=bodyprov)

summary(bp)
solve(crossprod(bp$family$data$R)) ## estimated cov matrix


plot(trial, pages=1)
plot(bp, pages = 1)

# solve(crossprod(bp$family$data$R)) ## estimated cov matrix

#0.01575861
#           [,1]       [,2]
#[1,] 0.08252208 0.01575861
#[2,] 0.01575861 0.79719820

#i think i might have to subset the data
#but low sample size might be an issue
postdeclinebp$BodyMass_z<-scale(postdeclinebp$newBodyMass)
postdeclinebp$prate_z<-scale(postdeclinebp$prate)
postdeclinebp$RightTarsus_z<-scale(postdeclinebp$RightTarsus)


#subsetted data and GLMM
thing<-postdeclinebp%>%
  group_by(BirdID)%>%
  summarize(count=length(BirdID))

bmfit<-bf(BodyMass_z~age_year+I(age_year)^2+newlifespan+RightTarsus_z+SexEstimate+avg_invert.y+
            newstat.y+(1|birthyear)+(1|q|BirdID))
prfit<-bf(prate_z~age_year+I(age_year)^2+newlifespan+SexEstimate+BroodSize+nr_helpers+
            avg_invert.x+newstat.x+(1|birthyear)+(1|q|BirdID))
fitprior <- get_prior(bmfit+prfit, data = postdeclinebp)
fitprior
fit<-brm(bmfit+prfit+set_rescor(TRUE), data = postdeclinebp, chains = 4,iter=10000,
         warmup = 1000, thin = 5, prior = fitprior)
fit
plot(fit)
summary(fit)

vcov(fit)

#in gamsfit#in gams
postdeclinebp$SexEstimate<-as.factor(postdeclinebp$SexEstimate)
postdeclinebp$BirdID<-as.factor(postdeclinebp$BirdID)
postdeclinebp$birthyear<-as.factor(postdeclinebp$birthyear)

postdeclinebp<-na.omit(postdeclinebp)

postbp<-gam(list(BodyMass_z~s(age_year, k=-1)+newlifespan+SexEstimate+newstat.x+
               avg_invert.x+RightTarsus_z+s(birthyear, bs='re')+s(BirdID, bs="re"),
             prate_z~ s(age_year, k=-1)+newlifespan+SexEstimate+newstat.y+BroodSize+
               nr_helpers+avg_invert.y+s(birthyear, bs='re')+s(BirdID, bs="re")),
        family=mvn(d=2),method="REML",data=postdeclinebp)

summary(postbp)
plot(postbp, pages=1)
solve(crossprod(postbp$family$data$R)) ## estimated cov matrix

        # [,1]         [,2]
# [1,]  0.042942958 -0.008214547
# [2,] -0.008214547  0.753117459
library(flextable)
as_flextable(postbp)





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

telofat<-filter(telofat, !is.na(telofat$RTL))
telofat<-na.omit(telofat)

telofat$RTL_z<-scale(telofat$RTL)

telofat$SexEstimate<-as.factor(telofat$SexEstimate)

sd(telofat$RTL_z)

prior<-list(R=list(V=diag(2)/2,nu=0.002),
            G=list(G1=list(V=diag(2)/2,nu=0.002),
                   G2=list(V=diag(2)/2, nu=0.002)))


# prior = prior1,
tfmcmc<-MCMCglmm(cbind(FatScore, RTL_z)~age_year+newlifespan+SexEstimate+
               newstat+avg_invert.x,
             random = ~us(1+age_year):BirdID + us(trait):birthyear, rcov=~us(trait):units,
             family = c('poisson', 'gaussian'), prior=prior,
             data=telofat, nitt = 1000000, burnin=5000, thin=100)

plot(tfmcmc)
autocorr(tfmcmc$VCV)
autocorr(tfmcmc$Sol)
summary(tfmcmc)

#attempt GLMM
#telo and fat score is linear
#can log transform fat score to make gaussian

View(telofat)


tfmod<-readRDS("tf_model.rds")
plot(tfmod)
autocorr(tfmod$VCV)
autocorr(tfmod$Sol)
summary(tfmod)



# #example model-- needs tweaking + priors and other variables
# #prior is wrong
# prior1<-list(G = list(G1 = list(v=diag(2), nu=0.002),
#                      G2 = list(V=diag(2), nu=0.002)),
#             R= list(R1=list(v=diag(2), nu=0.002),
#                     R2=list(v=diag(2), nu=0.002)))
#
# tf<-MCMCglmm(cbind(FatScore, RTL_z)~age_year+newlifespan+SexEstimate+newstat+avg_invert,
#          random = ~us(trait):BirdID + us(trait):birthyear, rcov=~us(trait):units,
#          family = c('poisson', 'gaussian'), prior = prior1,
#          data=telofat, nitt = 10000000, burnin=10000, thin=100)

# summary(tf)

#covar is -0.08


#I tried running this model with a gam bivariate and family=mvn(d=2)
#it turned out that the covariance was negative, which was unexpected since both gams came out with a linear negative slope
#possible that subset of data has changed the slope?
#when i plotted the model the slopes were wildly different than original models
#I then ran fat score separately in a gam with poisson and gaussian, the gaussian had a very different curve than poisson

# telofat2<-filter(telofat, !is.na(telofat$RTL_z))
#
# #simple test model
# test2<-gam(list(FatScore~s(age_year)+newlifespan,
#                 RTL_z~s(age_year)+newlifespan), family = mvn(d=2), data=telofat2, method='REML')
# plot(test2, pages = 1)
# solve(crossprod(test2$family$data$R))

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

#yes it does



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

telobuff2$newstat.x<-as.factor(telobuff2$newstat.x)
telobuff2$Whodunnit<-as.factor(telobuff2$Whodunnit)
telobuff2$birthyear<-as.factor(telobuff2$birthyear)
telobuff2$Observer<-as.factor(telobuff2$Observer)
telobuff2$BirdID<-as.factor(telobuff2$BirdID)
#can only compare in females
test<-gam(list(RTL_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
                 s(birthyear, bs='re')+s(BirdID, bs="re"),
               wbc_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
                 s(birthyear, bs='re')+s(BirdID, bs="re")),
          family = mvn(d=2), data = telobuff2, method="REML")


tb<-gam(list(RTL_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
               +s(Whodunnit, bs="re")+s(birthyear, bs='re')+s(BirdID, bs="re"),
         wbc_z~s(age_year)+newlifespan+avg_invert.x+newstat.x+
           s(Observer, bs='re')+s(birthyear, bs='re')+s(BirdID, bs="re")),
    family = mvn(d=2), data = telobuff2, method="REML")

solve(crossprod(test$family$data$R))


as_flextable(tb)

#0.0178214

############################
haemfat<- left_join(haemf, fatdata, by=c('BirdID',"age_year", 'newlifespan',
                                         'newstat','birthyear', "CatchID"))

haemfat<-filter(haemfat, !is.na(haemfat$FatScore))
haemfat$wbc_z<-scale(haemfat$wbc)
haemfat$FatScorelog<-log(haemfat$FatScore)

haemfit<-bf(wbc_z~age_year+I(age_year)^2+newlifespan+avg_invert.x+
              newstat+(age_year|q|BirdID))
fatfit<-bf(FatScorelog~age_year+I(age_year)^2+newlifespan+avg_invert.y+
             newstat+(age_year|q|BirdID))

hffit<-bf(mvbind(wbc_z,FatScorelog)~age_year+I(age_year)^2+newlifespan+
            avg_invert.x+newstat+(age_year|q|BirdID))

hfprior<-get_prior(haemfit+fatfit, data=haemfat)

trial<-brm(haemfit+fatfit+set_rescor(TRUE), data = haemfat,family=list(gaussian(),gaussian()),
           chains = 4,iter=10000, warmup = 100, thin = 10)

summary(trial)

hfprior<-c(set_prior('lkj(2)', class="b", resp='y1'),
           set_prior('student_t(3,0,2.5)', class = 'sigma', resp='y1'),
           set_prior('student_t(3, -0.1, 2.5)', class = 'intercept', resp='y1'),

)
mean(haemfat$FatScore)

hffit<-brm(haemfit+fatfit+set_rescor(TRUE), data = haemfat,
           chains = 4,iter=10000, warmup = 1000, thin = 5, prior = hfprior)

#######################################
#with MCMCglmm

mcprior<-list(R=list(V=diag(2)/0.5,nu=2),
            G=list(G1=list(V=diag(2)/0.5, nu=3)))

mcprior2<-list(R=list(V=diag(2)/2,nu=2),
               G=list(G1=list(V=diag(2)/2, nu=3)))

#G is for random effects, R is for residual


hfmcmc<-MCMCglmm(cbind(wbc_z,FatScore)~age_year+newlifespan+
                   avg_invert.x+newstat,
                 random = ~us(1+age_year):BirdID, rcov = ~us(trait):units,
         prior=mcprior,family=c('gaussian', 'poisson'),data=haemfat,
         nitt = 100000, burnin=5000, thin=50)

saveRDS(hfmcmc, 'haemfatmc.rds')

plot(hfmcmc)
summary(hfmcmc)
autocorr(hfmcmc$VCV)
autocorr(hfmcmc$Sol)
summary(tfmod)

cov_age_year <- hfmcmc$VCV[,c(5:8)]
colnames(cov_age_year)<-c('wbc','fatwbc','wbcfat','fat')
View(cov_age_year)


library(modelsummary)
library(broom.mixed)
thing<-tidy(tfmod)
tidyMCMC(thing)

library(flextable)



#this is for converting to observed scale - but i'm not sure if this works or not..
library(QGglmm)
library(purrr)
summary(hfmcmc)
# Formatting the output into lists
# flatten() is a purrr function that removes a level of nesting in a list
# e.g. it transforms list[[1]][[1]] into just list[[1]] which contains a vector
X <- hfmcmc[["X"]]
predict <- map(1:nrow(hfmcmc[["Sol"]]),
               ~ matrix(X %*% hfmcmc[["Sol"]][., ], ncol = 2))# Now, we can do the same with "VCV",
# but we need to format it into a matrix as well.
# Note grep("animal", ...) which collects only columns
# related to the "animal" effect.
G <-flatten(
    apply(hfmcmc[["VCV"]][ , grep("BirdID", colnames(hfmcmc[["VCV"]]))],
          1,
          function(row) {
            list(matrix(row, ncol = 2))
          })
  )
#the units
R <-flatten(
    apply(hfmcmc[["VCV"]][ , grep("units", colnames(hfmcmc[["VCV"]]))],
          1,
          function(row) {
            row[4] <- row[4] - 1
            list(matrix(row, ncol = 2))
          })
  )
# To obtain P we need to add G and R for each elements of their lists, easy:
P <- map2(G, R, `+`)

paramsM2 <-pmap(list(predict = predict, vcv.G = G,vcv.P = P),
       QGmvparams,
       models = c("Gaussian", "Poisson.log"),
       verbose = FALSE)

apply(paramsM2[["vcv.G.obs"]], c(1, 2), mean)
apply(paramsM2[["vcv.G.obs"]], c(1, 2), function (vec) { HPDinterval(as.mcmc(vec))[1] })
apply(paramsM2[["vcv.G.obs"]], c(1, 2), function (vec) { HPDinterval(as.mcmc(vec))[2] })
