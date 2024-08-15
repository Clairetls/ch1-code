# setwd('C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/')
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
survivalmodel<-readRDS("C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/survivalfinal2.rds")


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
bmdp<-ggplot(bmd, aes(age_year, derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = .20)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+
  labs(title = "(a) Body Mass")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
bmdp


dfunc<-function(x, term, title, ylim=c(-0.25,0.25)){
  der<-derivatives(x, term = term)
  term<-gsub("s\\(|\\)","",term)
  names(der)[names(der)==term]<-'data'
  derp<-ggplot(der, aes(data, .derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = .20)+ylim(ylim[1], ylim[2])+
    geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = title)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  derp
}

wlp<-dfunc(wing28mod, "s(age_year)","(b) Wing Length", c(-0.15,0.17))
wlp

tarp<-dfunc(tars28mod, "s(age_year)","(c) Tarsus Length", c(-0.05,0.05))

fatp<-dfunc(fat28mod, 's(age_year)', "(d) Fat Score", c(-0.1,0.05))
fatp


op<-dfunc(oxy28mod, 's(age_year)', "(f) Antioxidant Capacity", c(-0.25,0.25))
op


romp<-dfunc(rom28mod, 's(age_year)',"(e) Oxidants")
romp

hp<-dfunc(haem28mod, 's(age)',"(g) Haematocrit", ylim = c(-0.1,0.05))
hp


bd<-derivatives(buffy28mod)
bp<-ggplot(bd, aes(x = age, y = .derivative, color=SexEstimate)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = .lower_ci, ymax=.upper_ci), alpha = .20) +ylim(-0.5,0.25)+
  geom_line()+xlab("Age")+ylab("Derivative")+labs(title = "(h&i) Buffy Coat")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

bp

bdf<-filter(bd, bd$SexEstimate==0)
bdm<-filter(bd, bd$SexEstimate==1)


bpf<-ggplot(bdf, aes(x = age, y = .derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = .lower_ci, ymax=.upper_ci), alpha = .20) +ylim(-0.1,0.1)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "(h) Buffy Coat F")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

bpf


bpm<-ggplot(bdm, aes(x = age, y = .derivative)) + theme_bw(base_size =14) + theme_update(plot.title = element_text(hjust = 0.4))+
  geom_ribbon(aes(ymin = .lower_ci, ymax=.upper_ci), alpha = .20) +ylim(-0.5,0.25)+
  geom_line(color='#619CFF')+xlab("Age")+ylab("Derivative")+labs(title = "(i) Buffy Coat M")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(legend.position = "none")

bpm


tp<-dfunc(telo28mod, 's(age_year)', "(j) Relative TL", c(-0.08,0.02))
tp


pp<-dfunc(prov28mod, 's(age)', '(l) Provisioning rate',ylim = c(-0.2,0.1))
pp

mp<-dfunc(mal28mod, 's(age)',"(k) Malaria", ylim = c(-1,1.5))
mp

rp<-dfunc(rs28mod, 's(parent_age)', '(m) Annual fitness', ylim = c(-2.25,2.5))
rp


sp<-dfunc(survivalmodel, "s(age)","(n) Survival", ylim = c(-0.7,0.3))
sp

survd<-derivatives(survivalmodel)
View(survd)
rsd<-derivatives(rs28mod)
rsd<-filter(rsd, is.na(rsd$sex))

View(rsd)
library(gridExtra)


bigderiv<-grid.arrange(bmdp, wlp,tarp,fatp, romp,op, hp, bpf, bpm, tp,mp, pp, rp, sp,
                      ncol=3, nrow=5)

# bigderiv


##############################################################
library(flextable)

#create model summary tables
as_flextable(body_28mod)
as_flextable(wing28mod)
as_flextable(tars28mod)
as_flextable(fat28mod)
as_flextable(rom28mod)
as_flextable(oxy28mod)
as_flextable(haem28mod)
as_flextable(buffy28mod)
as_flextable(telo28mod)
as_flextable(mal28mod)
as_flextable(prov28mod)
as_flextable(rs28mod)
as_flextable(survivalmodel)

