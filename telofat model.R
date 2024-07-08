setwd("/home1/p309444/models/")

telofat<-read.csv("telofat.csv")
library(MCMCglmm)

telofat$RTL_z<-scale(telofat$RTL)
telofat<-na.omit(telofat)

# prior1<-list(G = list(G1 = list(v=diag(2)/2, nu=0.002),
#                       G2 = list(V=diag(2)/2, nu=0.002)),
#              R= list(R1=list(v=diag(2)/2, nu=0.002),
#                      R2=list(v=diag(2)/2, nu=0.002)))

prior<-list(R=list(V=diag(2)/2,nu=0.002),
       G=list(G1=list(V=diag(2)/2,nu=0.002),
              G2=list(V=diag(2)/2, nu=0.002)))

# prior = prior1,
tf<-MCMCglmm(cbind(FatScore, RTL_z)~age_year+newlifespan+SexEstimate+
               newstat+avg_invert.x,
             random = ~us(trait):BirdID + us(trait):birthyear, rcov=~us(trait):units,
             family = c('poisson', 'gaussian'), prior=prior,
             data=telofat, nitt = 100000000, burnin=50000, thin=100)


saveRDS(tf, "tf_model.rds")
