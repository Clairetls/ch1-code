setwd('C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/')
library(pracma)
# install.packages("gratia")
library(gratia)
library(ggplot2)
library(tidyverse)
library(mgcv)

files <- Sys.glob(file.path("*28mod.rds"))
gamlist<-lapply(files, readRDS)
names(gamlist)<-gsub(c(".rds"),"",files)
list2env(gamlist,envir=.GlobalEnv)
survivalmodel<-readRDS("survivalfinal.rds")


####################################################
# #model checks
#
# concurvity(body_28mod, full = F)
#
# summary(body_28mod)
#
# gam.check

####################################################


bmd<-derivatives(body_28mod, term = "s(age_year)")
bmdp<-ggplot(bmd, aes(data, derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .20)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "Body Mass")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
bmdp

dfunc<-function(x, term, title, ylim=c(-0.25,0.25)){
  der<-derivatives(x, term = term)
  derp<-ggplot(der, aes(data, derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .20)+ylim(ylim[1], ylim[2])+
    geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = title)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  derp
}

wlp<-dfunc(wing28mod, 's(age_year)',"Wing Length")


tard<-derivatives(tars28mod, term = "s(age_year)")
tarp<-ggplot(tard, aes(data, derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .20)+ylim(-0.05,0.05)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "Right Tarsus")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
tarp

fatd<-derivatives(fat28mod, term = "s(age_year)")
fatp<-ggplot(fatd, aes(data, derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .20)+ylim(-0.1,0.1)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "Fat Score")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
fatp

od<-derivatives(oxy28mod, term = "s(age_year)")
op<-ggplot(od, aes(x = data, y = derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha = .20) +ylim(-0.1,0.1)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "Antioxidant capacity")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

op

romp<-dfunc(rom28mod, 's(age_year)',"Oxidants")
romp

hp<-dfunc(haem28mod, 's(age)',"Haematocrit", ylim = c(-0.1,0.1))
hp


bd<-derivatives(buffy28mod)
bp<-ggplot(bd, aes(x = data, y = derivative, color=SexEstimate)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha = .20) +ylim(-0.5,0.25)+
  geom_line()+xlab("Age")+ylab("Derivative")+labs(title = "Buffy Coat")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

bp

td<-derivatives(telo28mod, term = "s(age_year)")
tp<-ggplot(td, aes(x = data, y = derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha = .20) +ylim(-0.08,0.02)+
  geom_line()+xlab("Age")+ylab("Derivative")+labs(title = "Telomere Length")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

tp


pp<-dfunc(prov28mod, 's(age)', 'Provisioning rate',ylim = c(-0.2,0.1))
pp

mp<-dfunc(mal28mod, 's(age)',"Malaria", ylim = c(-1,1.5))
mp

rp<-dfunc(rs28mod, 's(parent_age)', 'Annual fitness', ylim = c(-2.25,2.5))
rp

sp<-dfunc(survivalmodel, "s(age)","Survival", ylim = c(-0.3,0.1))
sp




library(gridExtra)


bigderiv<-grid.arrange(bmdp, wlp,tarp,fatp, romp,op, hp, tp,mp, pp, rp, sp, bp,
                      ncol=3, nrow=5)

# bigderiv


##############################################################

#
# rsf<-readRDS("C:/PhD/Data/output2/rs_f.rds")
# rsm<-readRDS("C:/PhD/Data/output2/rs_m.rds")
#
# plot(rsf, select=1)
# plot(rsm, select=1)


# rfp<-dfunc(rsf, 's(parent_age)', 'Annual fitness', ylim = c(-4,2.1))
# rfp
#
#
# rsmp<-dfunc(rsm, 's(parent_age)', 'Annual fitness', ylim = c(-1,2.1))
# rsmp
#



