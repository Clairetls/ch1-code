setwd('C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/')

physio<-read.csv("physio_28_5.csv",stringsAsFactors = F)
oxy<-read.csv("oxy_28_5.csv", stringsAsFactors = F)
haematocrit<-read.csv("haematocrit_28_5.csv",stringsAsFactors = F)
telomeres<-read.csv("telomere_28_5.csv", stringsAsFactors = F)
malaria<-read.csv("malaria_28_5.csv",stringsAsFactors = F, sep=';')
provisioning<-read.csv("provisioning_28_5.csv",stringsAsFactors = F, sep = ';',dec=',')
survival1<-read.csv("survival_model.csv",stringsAsFactors = F)
reprosuccess<-read.csv("repro_28_5.csv",stringsAsFactors = F)

files <- Sys.glob(file.path("*28mod.rds"))
gamlist<-lapply(files, readRDS)
names(gamlist)<-gsub(c(".rds"),"",files)
list2env(gamlist,envir=.GlobalEnv)
survivalmodel<-readRDS("survivalfinal.rds")

#####################################
m2_pred <- predict(m2, newdata = d.pred, se.fit = T)
d.pred <- mutate(d.pred,
                 y = m2_pred$fit,
                 ymin = m2_pred$fit - 1.96*m2_pred$se.fit,
                 ymax = m2_pred$fit + 1.96*m2_pred$se.fit)


##############33

plot(body_28mod,select=1,seWithMean = T,shade=TRUE, main="Body mass",
     ylab="Body mass",xlab="Age",xlim = c(0,17), ylim=range(physio$BodyMass_z, na.rm=T),
     cex.main=2, cex.lab=1.3)
points(physio$BodyMass_z~physio$age_year, physio, col=alpha("blue", 0.3))


plot(wing28mod,select=1,seWithMean = T,shade=TRUE, main="Wing Length",
     ylab="Wing Length",xlab="Age",xlim = c(0,17), ylim=range(physio$WingLength_z, na.rm=T),
     cex.main=2, cex.lab=1.3)
points(physio$WingLength_z~physio$age_year, physio, col=alpha("blue", 0.3))


plot(tars28mod,select=1,seWithMean = T,shade=TRUE, main="Right Tarsus",
     ylab="Right Tarsus",xlab="Age",xlim = c(0,17), ylim=range(physio$RightTarsus_z, na.rm=T),
     cex.main=2, cex.lab=1.3)
points(physio$RightTarsus_z~physio$age_year, physio, col=alpha("blue", 0.3))

physio$FatScore_z<-scale(physio$FatScore)

plot(fat28mod,select=1,seWithMean = T,shade=TRUE, main="Fat Score",
     ylab="Fat Score",xlab="Age",xlim = c(0,17), ylim=c(-4,6),
     cex.main=2, cex.lab=1.3)
points(jitter(physio$FatScore_z, 0.2)~physio$age_year, physio, col=alpha("blue", 0.3))

plot(haem28mod,select=1,seWithMean = T,shade=TRUE, main="Haematocrit",
     ylab="Haematocrit",xlab="Age",xlim = c(0,17), ylim=c(-4,5),
     cex.main=2, cex.lab=1.3)
points(jitter(haematocrit$Cells_z, 0.2)~haematocrit$age, haematocrit, col=alpha("blue", 0.3))


haemf<-filter(haematocrit, haematocrit$SexEstimate=0)
haemm<-filter(haematocrit, haematocrit$SexEstimate=1)

plot(buffy28mod,select=1,seWithMean = T,shade=TRUE, main="Buffy Coat",
     ylab="Buffy Coat",xlab="Age",xlim = c(0,20), ylim=c(-2,6),
     cex.main=2, cex.lab=1.3)
points(jitter(haemf$wbc_z, 0.2)~haemf$age, haemf, col=alpha("blue", 0.3))

plot(buffy28mod,select=2,seWithMean = T,shade=TRUE, main="Buffy Coat",
     ylab="Buffy Coat",xlab="Age",xlim = c(0,20), ylim=c(-2,6),
     cex.main=2, cex.lab=1.3)
points(jitter(haemm$wbc_z, 0.2)~haemm$age, haemm, col=alpha("blue", 0.3))


plot(oxy28mod,select=1,seWithMean = T,shade=TRUE, main="Antioxidants",
     ylab="Antioxidants",xlab="Age",xlim = c(0,17), ylim=c(-4,5),
     cex.main=2, cex.lab=1.3)
points(jitter(oxy$sum_oxy_z, 0.2)~oxy$age_year, oxy, col=alpha("blue", 0.3))

plot(rom28mod,select=1,seWithMean = T,shade=TRUE, main="Oxidants",
     ylab="Oxidants",xlab="Age",xlim = c(0,20), ylim=c(-2,6),
     cex.main=2, cex.lab=1.3)
points(jitter(oxy$sum_roms_z, 0.2)~oxy$age_year, oxy, col=alpha("blue", 0.3))


plot(telo28mod,select=1,seWithMean = T,shade=TRUE, main="Telomere Length",
     ylab="Telomere Length",xlab="Age",xlim = c(0,20), ylim=c(-2,6),
     cex.main=2, cex.lab=1.3)
points(jitter(telomeres$RTL_z, 0.2)~telomeres$age_year, telomeres, col=alpha("blue", 0.3))


plot(prov28mod,select=1,seWithMean = T,shade=TRUE, main="Provisioning Rate",
     ylab="Prov rate",xlab="Age",xlim = c(0,20), ylim=c(-2,6),
     cex.main=2, cex.lab=1.3)
points(jitter(provisioning$prate_z, 0.2)~provisioning$age, provisioning, col=alpha("blue", 0.3))

plot(mal28mod,select=1,seWithMean = T,shade=TRUE, trans= function(x) { exp(x) / (exp(x) + 1) }, main="Malaria",
     ylab="Malaria",xlab="Age",xlim = c(0,20), ylim=c(-0.1,1.1),
     cex.main=2, cex.lab=1.3)
points(jitter(malaria$TestResult, 0.2)~malaria$age, malaria, col=alpha("blue", 0.3))


plot(rs28mod,select=1,seWithMean = T,shade=TRUE, trans= exp, main="ARS",
     ylab="ARS",xlab="Age",xlim = c(0,20), ylim=c(-0.1,6),
     cex.main=2, cex.lab=1.3)
points(jitter(reprosuccess$ars,0.4)~reprosuccess$parent_age, reprosuccess, col=alpha("blue", 0.3))


plot(survivalmodel, select=1, trans= function(x) { exp(x) / (exp(x) + 1) }, shade = T, se = T, 
     ylab = "survival", xlab="age", ylim=c(-0.1,1.1), main="survival")
points(jitter(survival1$survival, 0.1)~survival1$age, col=alpha("blue", 0.3))

