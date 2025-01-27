setwd("C:/PhD/Data")


#install.packages("mgcv")
#install.packages("chron")
library(mgcv)
library(RODBC)
library(tidyverse)
library(chron)
library(lubridate)
library(rmarkdown)
#install.packages("tidymv")
library(tidymv)
#install.packages("tidygam")
library(tidygam)

"%!in%"<-Negate("%in%")

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:/db2/SeychellesWarbler1.11beta.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

#sex, cleaning individuals that have unclear sex 
sex<-read.csv("sys_SexEstimates.csv", sep = ";", stringsAsFactors = F)


#code used to remove sex conflict 
# conflict<-filter(sex,sex$DNAconflict==-1)
# conflict$m<-str_sub(conflict$DNAconflictRatio,5)
# conflict$m<-gsub("M:","",conflict$m)
# conflict$f<-str_sub(conflict$DNAconflictRatio,3,4)
# conflict$f<-as.numeric(conflict$f)
# conflict$m<-as.numeric(conflict$m)
# 
# conflict$certainty<-conflict$f/conflict$m
# conflict$certainty_f<-conflict$f/(conflict$m+conflict$f)
# conflict$certainty_m<-conflict$m/(conflict$m+conflict$f)

discard<-c(199,530,984,1230,1397,1508,1580,1619,1620,1675,1705,1806,3456,
           3531,3550,3616,3622,3629,3654,3662,3673,3674,3679,3681,3687,5256)
sex<-filter(sex, sex$BirdID %!in% discard)
sex<-sex[,c("BirdID","SexEstimate")]

#birthdate and fieldperiod 
birthfp<-sqlQuery(swdb,"SELECT tblBirdID.BirdID, tblBirdID.BirthFieldPeriodID
FROM tblBirdID WHERE (((tblBirdID.BirdID)>0));", stringsAsFactors=F)F

#loading data 
physio<-read.csv("physiological.csv", stringsAsFactors = F)
physio<-physio[,-(1)]
tquali<-read.table("sys_TerritoryQuality.csv", stringsAsFactors=F, sep = ";", dec = ",", header = T)
lifespan<-read.csv("maxlifespan.csv", stringsAsFactors = F, sep = ";")
fps<-readxl::read_excel("fps.xlsx")
tquali<-filter(tquali, tquali$Island=="CN")
tquali<-tquali[,c("Island", "Year", "TerritoryID","TerritoryNumber","FieldPeriodID","TQ","TQcorrected")]
#tq<-tquali[,c("Island","Year","TerritoryID","TerritoryNumber","FieldPeriodID","TQ")]

#breed group location and breed status
brgp_status_location<-sqlQuery(swdb, "SELECT tblBreedStatus.BreedGroupID, tblBreedStatus.BirdID, tblBreedStatus.Status, tblBreedStatus.Observer, tblBreedGroupLocation.FieldPeriodID, tblBreedGroupLocation.TerritoryID, tblFieldPeriodIDs.PeriodStart, tblFieldPeriodIDs.PeriodEnd, tblFieldPeriodIDs.PeriodYear
FROM tblFieldPeriodIDs INNER JOIN (tblBreedGroupLocation INNER JOIN 
tblBreedStatus ON tblBreedGroupLocation.BreedGroupID = tblBreedStatus.BreedGroupID) ON tblFieldPeriodIDs.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID;", stringsAsFactors=F)
 

birdstatus<-sqlFetch(swdb, "sys_StatusByFieldPeriod", stringsAsFactors=F)
            
status_terr_brgp<-left_join(birdstatus, brgp_status_location, by.x=c("BirdID","FieldPeriodID"),all.x=T)        

#sighted in more than 1 territory 

#take the territory they were dominant in 
status_terr_brgp$TerritoryID[status_terr_brgp$TerritoryID<=0]<-NA
# arsdf$newstat[arsdf$Status %in% c("BrF","BrM","BrU")]<-"Dom"

  # filter(TerritoryID>0)

status_terr_brgp<-status_terr_brgp%>%
  group_by(BirdID, FieldPeriodID)%>%
  mutate(gpno=length(unique(BreedGroupID)), 
         terrno=length(unique(TerritoryID)))



#insect abundance
bugs<-read.csv("usys_qTerrQualityInsectsPerDM2.csv", sep=";", stringsAsFactors = F, dec = ",")
bugs<-filter(bugs, bugs$Island=="CN")
#bugs[,c(5:47)]<-as.numeric(bugs[,c(5:47)])

bugs$invertsum<-rowSums(bugs[,c(29:47)], na.rm=T)
total_invert<-bugs[,c("Island","Year","FieldPeriodID","Location","invertsum")]
total_invert<-total_invert%>%
  group_by(Year, FieldPeriodID)%>%
  mutate(avg_invert=mean(invertsum))
total_invert$occasionyear<-total_invert$Year
total_invert<-total_invert[,c("occasionyear", "FieldPeriodID","avg_invert")]
total_invert<-unique(total_invert)



# bugs2<-read.csv("usys_qTerrQualityInsectsPerDM2_new.csv", sep=";", stringsAsFactors = F, dec = ",")
# bugs2<-bugs2%>% filter(Island=="CN")
# #bugs[,c(5:47)]<-as.numeric(bugs[,c(5:47)])
# 
# bugs2$invertsum<-rowSums(bugs2[,c(29:47)], na.rm=T)
# total_invert2<-bugs2[,c("Island","Year","FieldPeriodID","Location","invertsum")]
# total_invert2<-total_invert2%>%
#   group_by(Year, FieldPeriodID)%>%
#   mutate(avg_invert=mean(invertsum))
# 
# 
# total_invert2<-as.data.frame(total_invert2)
# total_invert2$occasionyear<-total_invert2$Year
# total_invert2<-total_invert2[,c("occasionyear", "FieldPeriodID","avg_invert")]
# total_invert2<-unique(total_invert2)

write.csv(total_invert, "mean_insect_new.csv")

#function for between and within age 
AveByInd <- function(x) mean(x)


#within age centering 
WithinIndCentr <- function(x) x-mean(x)



#early life territory (and quality)
pedigree_earlylife<-sqlQuery(swdb, "SELECT sys_PedigreeCombined.BirdID, sys_PedigreeCombined.BirthDate, sys_PedigreeCombined.NestID, tblNestInfo.BreedGroupID, tblBreedGroupLocation.FieldPeriodID, tblBreedGroupLocation.TerritoryID
FROM tblBreedGroupLocation INNER JOIN (sys_PedigreeCombined INNER JOIN tblNestInfo ON sys_PedigreeCombined.NestID = tblNestInfo.NestID) ON tblBreedGroupLocation.BreedGroupID = tblNestInfo.BreedGroupID;
", stringsAsFactors=F)


#breedgroup, status and location 
brgp_status_location$Status<-as.character(brgp_status_location$Status)

rmvstat<-c("BrF","BrM","BrU","H", 'A', "NS","TBRM","TBRF","SBR","A","NSA")
keepstat<-c("FL","OFL","CH","SA","XF")

# theothers<-filter(brgp_status_location, brgp_status_location$Status %in% keepstat)
#theothers<-filter(brgp_status_location, brgp_status_location$Status!="BrM")


# extra<-filter(theothers, theothers$BirdID %!in% pedigree_earlylife$BirdID)
# extra<-filter(extra, extra$BirdID>0)
# 
# extrabirds<-extra[,c("BirdID", "BreedGroupID", "TerritoryID", "FieldPeriodID","Status")]
# 
# test<-extra[,c("BirdID", "BreedGroupID", "TerritoryID")]
# 
# test2<-test%>%
#   group_by(BirdID)%>%
#   summarize(terrcount=length(unique(TerritoryID)))


#body mass model 

#first standardize the trait 
physio$BodyMass_z<-(physio$BodyMass-mean(physio$BodyMass, na.rm=T))/sd(physio$BodyMass, na.rm=T)
physio$RightTarsus_z<-(physio$RightTarsus-mean(physio$RightTarsus, na.rm=T))/sd(physio$RightTarsus, na.rm=T)
physio<-merge(physio, lifespan, by.x = "BirdID", all.x = T)
physio<-left_join(physio,tquali, by.x = c('TerritoryID',"FieldPeriodID"), all.x = T)
physio<-merge(physio, sex, by.x = "BirdID", all.x = T) #no conflicted-sex birds
#physio$TQcorrected<-as.numeric(physio$TQcorrected)

physio<-filter(physio, physio$age_year>=1)
physio$catch_time<-chron(times=physio$CatchTime)
physio$CatchTime_mins<-60 * 24 * as.numeric(times(physio$catch_time))
physio$WingLength_z<-scale(physio$WingLength,center = T)
physio$FatScore_z<-scale(physio$FatScore)

# #between age
# physio_age<-do.call("rbind", as.list(
#   by(physio, physio["BirdID"], transform, AveAge=AveByInd(age_year))))
# 
# #within age 
# physio <- do.call("rbind", as.list(
#   by(physio_age, physio_age["BirdID"], transform, WithinAge=WithinIndCentr(age_year))))

physio<-physio[-c(3950),]
physio<-physio[-c(1001),]  #remove the female age 9 weight 20.2 


physio<-physio[,!names(physio)=="Year"]

#merging yearly insect with phys
total_invert<-as.data.frame(total_invert)
# total_invert$occasionyear<-total_invert$Year
# total_invert<-total_invert[,c("occasionyear", "FieldPeriodID","avg_invert")]
# total_invert<-unique(total_invert)
physio<-left_join(physio, total_invert, all.x=T)

physio<-physio%>%
  mutate(newstat=case_when(Status == "BrF" ~ "Dom",
                           Status == "BrM" ~ "Dom",
                           Status == "BrU" ~ "Dom"))
  
physio$newstat[physio$Status %in% c("U", "SEEN1", "SEEN2","TBrM","TBrF","AB",'ABX','FLOAT','H','OFL','B')] <- "Sub"

#something in lubridate to convert
physio$Observer<-as.factor(physio$Observer)
physio$BirdID<-as.factor(physio$BirdID)
physio$SexEstimate<-as.factor(physio$SexEstimate)
physio$Status<-as.factor(physio$Status)

write.csv(physio,"physio_model.csv")


# ggplot(physio, aes(x=age_year, y=WingLength))+geom_point(alpha=0.2)+
#   geom_smooth(method="gam")+xlab("Age")+ylab("Wing Length")+theme_bw()


#manytrialslater

#i guess this kinda works but idk 

# p_df<-data.frame(x=physio$age_year, y=physio$WingLength)
# physio$p_wing<-predict(winglength, newdata=physio)
# 
# 
# p2<-ggplot() +geom_point(physio, aes(x = age_year, y = WingLength), color = "black") 
#   
# plot1<-ggplot(physio, aes(x=age_year, y=p_wing))+geom_smooth()
# plot1+ geom_point(aes(x = age_year, y = WingLength_z), color = "black") 
# plot1

###################################

#what does the se with mean term mean
# points(physio$age_year, physio$WingLength, pch=16, col="black")
# 
# 
# plot(wing2,select=1,seWithMean=2,trans=exp,shade=TRUE,shift=coef(wing2)[1],main="Wing Length",
#      ylab="Wing Length",xlab="Age") 

#what is categorical variable 
#need to rerun
#dont have to specify as s(), just +sex estimate (as factor) 

# ggplot(physio, aes(x=age_year, y=RightTarsus))+geom_smooth(method = "gam")+geom_point(alpha=0.2)


# nrow(physio)-sum(is.na(physio$BodyMass))
# ggplot(physio, aes(x=age_year, y=FatScore))+geom_point(position="jitter" ,alpha=0.2)+
#   geom_smooth(method = "gam")


#Annual reproductive success 
arsdf<-read.csv("annualreprosuccess.csv", stringsAsFactors = F)

arsdf<-arsdf[,-c(1)]

arsdf<-merge(arsdf,lifespan, by.x="BirdID",all.x = T)

agefirstlastrepro<-arsdf%>%
  filter(arsdf$ars>0)%>%
  group_by(BirdID)%>%
  summarise(minrepro=min(chickyear), maxrepro=max(chickyear))

arsdf<-merge(arsdf, agefirstlastrepro, by.x = "BirdID", all.x = T)
arsdf$minreproage<-arsdf$minrepro-arsdf$birthyear
arsdf$maxreproage<-arsdf$maxrepro-arsdf$birthyear

status_terr_brgp_yr<-left_join(status_terr_brgp, fps)
status_terr_brgp_yr$PeriodStart<-as.Date(status_terr_brgp_yr$PeriodStart, "%Y-%m-%d")
status_terr_brgp_yr$PeriodEnd<-as.Date(status_terr_brgp_yr$PeriodEnd, "%Y-%m-%d")


status_terr_brgp_yr$newstat[status_terr_brgp_yr$Status %in% c("BrF","BrM","BrU")]<-"Dom"
status_terr_brgp_yr$newstat[status_terr_brgp_yr$Status %!in% c("BrF","BrM","BrU")]<-"Sub"
status_terr_brgp_yr$newstat[is.na(status_terr_brgp_yr$Status)==T]<-NA


statperyr<-data.frame()

statperyr<-status_terr_brgp_yr%>%
  group_by(BirdID,PeriodYear)%>%
  arrange(PeriodYear,factor(Status, levels = c("Dom","Sub"))) %>%
  filter(!duplicated(PeriodYear))

# statperyr<-status_terr_brgp_yr%>%
#   group_by(BirdID, PeriodYear)%>%
#   filter(PeriodEnd==max(PeriodEnd))%>%
#   rbind(statperyr)

# statperyr$BirdID<-as.numeric(statperyr$BirdID)
statperyr$chickyear<-statperyr$PeriodYear

arsdf2<-left_join(arsdf, statperyr, by.x=c("BirdID","chickyear"))

# st<-arsdf2%>%
#   group_by(BirdID,PeriodYear)%>%
#   summarise(stno=length(unique(Status)))

# arsdf$newstat[arsdf$Status %in% c("BrF","BrM","BrU")]<-"Dom"
# arsdf$newstat[arsdf$Status %in% c("AB","SEEN1","FL","ABX","NSA",
#                                 "H","U","CH","NS","XF","SEEN2","OFL",
#                                 "B","SBR")]<-"Sub"


 #extra rows still resulting from epp individuals and floaters 
#i really dont know what to do




arsdf2<-left_join(arsdf2,tquali, by.x=c("FieldPeriodID","TerritoryID"), all.x = T)

#yearly fledgling 
birdid<-sqlQuery(swdb,"SELECT tblBirdID.BirdID, tblBirdID.BirthDate, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.FieldPeriodID
FROM tblFieldPeriodIDs INNER JOIN tblBirdID ON tblFieldPeriodIDs.FieldPeriodID = tblBirdID.BirthFieldPeriodID;
",stringsAsFactors=F)
birdid<-filter(birdid, birdid$Island=="CN")
birdid$byear<-as.numeric(str_sub(birdid$BirthDate,1,4))



nofledge<-sqlQuery(swdb,"SELECT tblNestInfo.NestID, tblNestInfo.BreedGroupID, tblNestInfo.NoFledglings, tblBreedGroupLocation.FieldPeriodID, tblBreedGroupLocation.TerritoryID, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.PeriodEnd, tblFieldPeriodIDs.PeriodYear
FROM tblFieldPeriodIDs INNER JOIN (tblBreedGroupLocation INNER JOIN tblNestInfo ON tblBreedGroupLocation.BreedGroupID = tblNestInfo.BreedGroupID) ON tblFieldPeriodIDs.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID;
", stringsAsFactors=F)

nofledge<-filter(nofledge, nofledge$Island=="CN")
nofledge<-nofledge|>
  group_by(PeriodYear)|>
  mutate(yearly_fl=sum(NoFledglings, na.rm = T))

numfl<-unique(nofledge[,c(8,9)])


#test
# test<-survival%>%
#   group_by(BirdID, PeriodYear)%>%
#   summarize(st=length(Status))


yearlings<-birdid%>%
  group_by(byear)%>%
  summarise(flno=length(BirdID))
yearlings<-as.data.frame(yearlings)
names(yearlings)[names(yearlings)=="byear"]<-"chickyear"
yearlings$chickyear<-as.numeric(yearlings$chickyear)

arsdf2<-left_join(arsdf2,yearlings, by.x=c("chickyear"),all.x=T)

# rsdf2<-filter(rsdf, rsdf$terrno<2)
# write.csv(rsdf, "reprosuccess_model.csv")
write.csv(arsdf2, "reprosuccess_model.csv")





#latest repro success using field period 
rsdf<-read.csv("reprosuccess.csv", stringsAsFactors = F)

rsdf<-rsdf[,-c(1)]

rsdf<-merge(rsdf,lifespan, by.x="BirdID",all.x = T)
rsdf$FieldPeriodID<-rsdf$BirthFieldPeriodID

agefirstlastrepro<-rsdf%>%
  filter(rsdf$rs>0)%>%
  group_by(BirdID)%>%
  summarize(minrepro=min(chickyear), maxrepro=max(chickyear))

rsdf<-merge(rsdf, agefirstlastrepro, by.x = "BirdID", all.x = T)
rsdf$minreproage<-rsdf$minrepro-rsdf$birthyear
rsdf$maxreproage<-rsdf$maxrepro-rsdf$birthyear

rsdf<-left_join(rsdf,status_terr_brgp, by.x=c("BirdID","FieldPeriodID"),all.x=T)
#extra rows still resulting from epp individuals and floaters 
#i really dont know what to do


rsdf$newstat[rsdf$Status %in% c("BrF","BrM")]<-"Dom"
rsdf$newstat[rsdf$Status %in% c("AB","SEEN1","FL","ABX","NSA",
                                "H","U","CH","NS","XF","SEEN2","OFL",
                                "B","SBR")]<-"Sub"

rsdf<-left_join(rsdf,tquali, by.x=c("FieldPeriodID","TerritoryID"), all.x = T)

#yearly fledgling 
birdid<-sqlQuery(swdb,"SELECT tblBirdID.BirdID, tblBirdID.BirthDate, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.FieldPeriodID
FROM tblFieldPeriodIDs INNER JOIN tblBirdID ON tblFieldPeriodIDs.FieldPeriodID = tblBirdID.BirthFieldPeriodID;
",stringsAsFactors=F)
birdid<-filter(birdid, birdid$Island=="CN")
birdid$byear<-as.numeric(str_sub(birdid$BirthDate,1,4))



nofledge<-sqlQuery(swdb,"SELECT tblNestInfo.NestID, tblNestInfo.BreedGroupID, tblNestInfo.NoFledglings, tblBreedGroupLocation.FieldPeriodID, tblBreedGroupLocation.TerritoryID, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.PeriodEnd, tblFieldPeriodIDs.PeriodYear
FROM tblFieldPeriodIDs INNER JOIN (tblBreedGroupLocation INNER JOIN tblNestInfo ON tblBreedGroupLocation.BreedGroupID = tblNestInfo.BreedGroupID) ON tblFieldPeriodIDs.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID;
", stringsAsFactors=F)

nofledge<-filter(nofledge, nofledge$Island=="CN")
nofledge<-nofledge|>
  group_by(PeriodYear)|>
  mutate(yearly_fl=sum(NoFledglings, na.rm = T))

numfl<-unique(nofledge[,c(8,9)])


#test
# test<-survival%>%
#   group_by(BirdID, PeriodYear)%>%
#   summarize(st=length(Status))


yearlings<-birdid%>%
  group_by(byear)%>%
  summarise(flno=length(BirdID))
yearlings<-as.data.frame(yearlings)
names(yearlings)[names(yearlings)=="byear"]<-"chickyear"
yearlings$chickyear<-as.numeric(yearlings$chickyear)

rsdf<-left_join(rsdf,yearlings, by.x=c("chickyear"),all.x=T)

rsdf2<-filter(rsdf, rsdf$terrno<2)
write.csv(rsdf, "reprosuccess_model.csv")
write.csv(rsdf2, "rs_nodupterr.csv")

test<-filter(rsdf, rsdf$terrno>1)

#yearling 2
rsdf2<-read.csv("rs_nodupterr.csv")
terr<-sqlQuery(swdb,"SELECT tblTerritories.TerritoryID, tblTerritories.TerritoryNumber, tblTerritories.Island
FROM tblTerritories
WHERE (((tblTerritories.TerritoryID)>0));
", stringsAsFactors=F)
terr<-filter(terr,terr$Island=="CN")
natalterr<-read.csv("Pedfinal_withNatalTerr.csv",stringsAsFactors = F)

names(natalterr)[names(natalterr)=='NatalTerritory']<-"TerritoryNumber"
names(natalterr)[names(natalterr)=="NatalFPID"]<-"FieldPeriodID"
names(natalterr)[names(natalterr)=='id']<-"BirdID"

natalterr<-merge(natalterr, terr, by.x="TerritoryNumber", all.x=T)

natalterr<-left_join(natalterr, breedgrouplocation, by.x=c("BirdID","FieldPeriodID","TerritoryID"), all.x=T)

natalterr<-natalterr%>%
  group_by(TerritoryID,FieldPeriodID)%>%
  mutate(NoFledglings=length(unique(BirdID)))

# missing2<-

pedinfo<-unique(natalterr[,c("TerritoryID","NoFledglings","FieldPeriodID")])
nestinfo<-unique(nofledge[,c("TerritoryID","NoFledglings","FieldPeriodID")])

missing<-anti_join(pedinfo,nestinfo)

fpyear<-sqlQuery(swdb,"SELECT tblFieldPeriodIDs.FieldPeriodID, tblFieldPeriodIDs.PeriodYear
FROM tblFieldPeriodIDs;",stringsAsFactors=F)
missing<-merge(missing,fpyear, by.x=c("FieldPeriodID"), all.x=T)

missingfl<-missing%>%
  group_by(PeriodYear)%>%
  summarise(nofl=sum(NoFledglings))

names(numfl)[names(numfl)=="yearly_fl"]<-"nofl"
flflflfl<-rbind(missingfl, numfl)
fledge<-flflflfl%>%
  group_by(PeriodYear)%>%
  summarise(yearfl=sum(nofl))

rsdf2<-merge(rsdf2, fledge, by.x = "PeriodYear", all.x=T)

write.csv(rsdf2, "rs_flbynestinfo.csv")
#old code
ARS<-read.csv("annualreprosuccess.csv", stringsAsFactors=F, sep = ",")
#annualrepro<-read.csv("ars.csv", stringsAsFactors = F, sep = ";")
ARS<-ARS[,-c(1)]
ARS<-filter(ARS, ARS$chickyear<=2017) #remove anything past summer 2017

#obtain age of first and last reproduction 
agefirstlastrepro<-ARS%>%
  filter(ars>0)%>%
  group_by(BirdID)%>%
  summarize(minrepro=min(chickyear), maxrepro=max(chickyear))

ARS<-merge(ARS, agefirstlastrepro, by.x = "BirdID", all.x = T)
ARS$minreproage<-ARS$minrepro-ARS$birthyear
ARS$maxreproage<-ARS$maxrepro-ARS$birthyear


#status per year 
new_query<-sqlQuery(swdb, "SELECT sys_StatusByFieldPeriod.BirdID, sys_StatusByFieldPeriod.Status, sys_StatusByFieldPeriod.FieldPeriodID, tblFieldPeriodIDs.PeriodStart, tblFieldPeriodIDs.PeriodEnd, tblFieldPeriodIDs.PeriodYear
FROM sys_StatusByFieldPeriod INNER JOIN tblFieldPeriodIDs ON sys_StatusByFieldPeriod.FieldPeriodID = tblFieldPeriodIDs.FieldPeriodID;
", stringsAsFactors=F)

new_query$chickyear<-new_query$PeriodYear

statusperyear<-data.frame()

for(i in unique(new_query$BirdID)){
  onebird<-filter(new_query, new_query$BirdID==i)
  for(i in unique(onebird$PeriodYear)){
    year<-filter(onebird, onebird$PeriodYear==i)
    mainseason<-filter(year, year$PeriodEnd==max(year$PeriodEnd))
    statusperyear<-rbind(mainseason,statusperyear)
  }
}

ARS_status<-merge(ARS, statusperyear, by.x=c("BirdID","chickyear"), all.x=T)
ARS_status<-merge(ARS_status, lifespan, by.x = "BirdID", all.x = T)

#status field period id year, first last, 

  #how many periods in the year 
  #if one, then status= that one status 
  #if more than one, take status of main breeding season. 
  

# for(i in unique(new_query$PeriodYear)){
#   year<-filter(new_query, new_query$PeriodYear==i)
#   mainseason<-filter(year, year$PeriodEnd==max(year$PeriodEnd))
#   statusterritory<-rbind(mainseason,statusterritory)
# }

# statusterritory$chickyear<-statusterritory$PeriodYear
# 


#assign bird a breeding status 
# ARS_stat<-merge(ARS, statusterritory, by.x=c("BirdID","chickyear"), all.x=T)



#link the bird to the territory? 
#brgp_status_location$chickyear<-brgp_status_location$PeriodYear
brgp_status_location<-filter(brgp_status_location, brgp_status_location$TerritoryID>0)
brgp_status_location<-filter(brgp_status_location, brgp_status_location$BirdID>0)
#brgp_status_location<-filter(brgp_status_location, brgp_status_location$Status %!in% keepstat)

#find no of helpers per group 
brgp_status_location<-brgp_status_location%>%
  group_by(BreedGroupID)%>%
  mutate(groupsize=length(BreedGroupID))%>%
  mutate(nr_helpers=sum(Status=="H"))
brgp_status_location<-brgp_status_location[,-c(4)]

brgp_status_location<-as.data.frame(brgp_status_location)

#brgp_status_location<-brgp_status_location[,-c(3,4)]

test<-left_join(ARS_status, brgp_status_location, by.x = c("BirdID", "FieldPeriodID"), all.x=T)

#you probably have more than one breed group per year. 
yaumud9ah<-test%>%
  group_by(BirdID, PeriodYear)%>%
  summarize(countbg=length(unique(BreedGroupID)))
  
#birds with multiple breed groups in a year exist 


#provisioning model 

#get breed group size 

brgrpstatus<-brgpstatus%>%
  group_by(BreedGroupID)%>%
  mutate(groupsize=length(BreedGroupID))

#merge provi rate with group size, field period id and territory id 
provisioning<-read.csv("provisioning.csv", stringsAsFactors = F)
provisioning<-left_join(provisioning, brgp_status_location, by.x = c("BreedGroupID", "BirdID"), all.x=T)
provisioning<-provisioning[,-c(1)]

provis<-left_join(provisioning, tquali, by.x = c("TerritoryID","FieldPeriodID"), all.x = T)

provis$prate_z<-scale(provis$prate, center = T)
provis<-merge(provis, lifespan, by.x = "BirdID", all.x = T)
provis$lifespan_yrs<-provis$lifespan/365

provis <- do.call("rbind", as.list(
  by(provis, provis["BirdID"], transform, AveAge=AveByInd(age))))


#within age centering 
provis <- do.call("rbind", as.list(
  by(provis, provis["BirdID"], transform, WithinAge=WithinIndCentr(age))))

provis$lifespan_yrs<-as.numeric(provis$lifespan_yrs)
provis$newstat[provis$Status %in% c("BrF","BrM")]<-"Dom"
provis$newstat[provis$Status %in% c("H","ABX","SEEN1",'TBRF','TBRM','SBR',NA)]<-"Sub"

provis<-left_join(provis, total_invert, by.x=c("FieldPeriodID"),all.x=T)
provis<-left_join(provis, sex, by.x=c("BirdID"),all.x=T)


watchtype<-sqlQuery(swdb, "SELECT tblNestWatchDetails.NestWatchID, tblNestWatchDetails.NestID, tblNestWatchDetails.WatchType
FROM tblNestWatchDetails;
",stringsAsFactors=F)

provis<-left_join(provis, watchtype, by.x=c("NestWatchID", "NestID"), all.x=T)


write.csv(provis, "provisioning_model.csv")

provis$BirdID<-as.factor(provisioning$BirdID)
provis$Status<-as.factor(provisioning$Status)

#needs number of helpers 
#
provrate<-gam(prate_z~s(age_years, k=16)+ lifespan+ BroodSize+ groupsize+
                s(Status, bs="fs")+s(TQ)+s(BirdID, bs = "re"), 
            data = provis, method = "REML", family = gaussian)


#lets see if TQ is fixed.. i think so. 
#why do i have to add nest watch type?? 
#went through all the nest watch tables in db--nest watch type doesnt exist?  

summary(provrate)

plot(provrate,select=1,seWithMean=2,shade=TRUE,shift=coef(provrate)[1],main="Provisioning rate",
     ylab="Provisioning rate",xlab="Age",xlim = c(0,20), ylim = c(-4,6))

ggplot(provis, aes(x=age_days, y=prate_z))+geom_point()+
  stat_smooth(method = "gam", formula = y~s(x,k=16))+ylim(-2,6)+
  xlim(0,7000)+ ylab("Provisioning Rate")+xlab("Age in days")+theme_bw()


####################
#the blood work

#oxidative stress- oxy are antioxidants and roms are oxidants 
oxy<-read.csv('oxidativestress.csv', stringsAsFactors = F)

toscale<-c("dROMsAbs505nm","dROMsCARRU505nm","dROMs505mMolH2O2","dROMsAbs546nm",
           "dROMsCARRU546nm","dROMs546mMolH2O2","OXYabs505nm",
           "OXYmMolHClO505nm","OXYabs546nm","OXYmMolHClO546nm")
for(i in toscale){
  scaled<-scale(oxy[,i])
  colnames(scaled)<-paste(i, '_z')
  oxy<-cbind(oxy,scaled)
  }

oxy<-filter(oxy, oxy$OXYanalysed==1)
catchplusstatus<-sqlFetch(swdb, "sys_CatchPlusStatus", stringsAsFactors)
catchplusstatus<-catchplusstatus[,c(1:11)]

oxy$sum_roms_z<-oxy$`dROMs505mMolH2O2 _z`+oxy$`dROMs546mMolH2O2 _z`+
  oxy$`dROMsAbs505nm _z`+oxy$`dROMsAbs546nm _z`+oxy$`dROMsCARRU505nm _z`+
  oxy$`dROMsCARRU546nm _z`

oxy$sum_roms<-oxy$dROMs505mMolH2O2+oxy$dROMs546mMolH2O2+oxy$dROMsAbs505nm+
  oxy$dROMsAbs546nm+oxy$dROMsCARRU505nm+oxy$dROMsCARRU546nm
oxy$sum_roms_scale<-scale(oxy$sum_roms)

oxy$sum_oxy<-oxy$OXYabs505nm+oxy$OXYabs546nm+oxy$OXYmMolHClO505nm+oxy$OXYmMolHClO546nm
oxy$sum_oxy_scale<-scale(oxy$sum_oxy)
oxy$sum_oxy_z<-oxy$`OXYabs505nm _z`+oxy$`OXYabs546nm _z`+oxy$`OXYmMolHClO505nm _z`+oxy$`OXYmMolHClO546nm _z`

oxy$oxyratio<-oxy$sum_oxy/oxy$sum_roms

oxy$OccasionDate<-as.Date(oxy$OccasionDate, "%Y-%m-%d")
catchplusstatus$OccasionDate<-as.Date(catchplusstatus$OccasionDate, "%Y-%m-%d")
oxy<-left_join(oxy, catchplusstatus, by.x = c("CatchID", "BirdID"), all.x = T)

oxy<-left_join(oxy,tquali, by.x = c("TerritoryID","FieldPeriodID"), all.x=T)

oxy<-left_join(oxy, lifespan, by.x="BirdID", all.x=T)
oxy$lifespan_yrs<-oxy$lifespan/365

oxy<- left_join(oxy, sex, by.x="BirdID", all.x=T)

oxy <- do.call("rbind", as.list(
  by(oxy, oxy["BirdID"], transform, AveAge=AveByInd(age_year))))


#within age centering 
oxy <- do.call("rbind", as.list(
  by(oxy, oxy["BirdID"], transform, WithinAge=WithinIndCentr(age_year))))

oxy<-oxy[,-c(1)]

oxy$newstat[oxy$Status %in% c("BrF", "BrM","BrU")]<-"Dom"
oxy$newstat[oxy$Status %in% c("ABX","TBrF","H","FLOAT","AB","SEEN1","OFL",
                              "SEEN2","B","FL","U")]<-"Sub"

oxy<-merge(oxy, sex, by.x = "BirdID", all.x = T)
oxy$CatchTime<-str_sub(oxy$CatchTime, 12,19)
oxy$CatchTime<-chron(times=oxy$CatchTime)
oxy<-merge(oxy, total_invert, by.x = c("occasionyear","FieldPeriodID"),all.x = T)

oxy<-left_join(oxy, brgp_status_location, by.x=c("BirdID", "FieldPeriodID"), all.x=T)

write.csv(oxy, "oxy_model.csv")



##haema
haematocrit<-read.csv("haematocrit.csv", stringsAsFactors = F)
haematocrit<-left_join(haematocrit, lifespan, by.x="BirdID", all.x=T)
haematocrit$OccasionDate<-as.Date(haematocrit$OccasionDate, "%Y-%m-%d")
haematocrit$birthday<-as.Date(haematocrit$birthday, "%Y-%m-%d")
haematocrit$age_date<-haematocrit$OccasionDate-haematocrit$birthday

haematocrit<-filter(haematocrit, haematocrit$Cells>0) #filtered out cells=0 because idk what that means and surely theres an error if cells = 0 
haematocrit$wbc<-haematocrit$BuffyCoat/haematocrit$Total

haemcatch<-catchplusstatus[,-c(3)]
haem2<-left_join(haematocrit, haemcatch, by.x=c("BirdID","CatchID"),all.x=T)
haem2<-left_join(haem2, sex, by.x="BirdID", all.x=T)

# haem2$BirdID<-as.factor(haem2$BirdID)
# haem2$Status<-as.factor(haem2$Status)
haem2$lifespan_yrs<-haem2$lifespan/365
haem2<-filter(haem2, haem2$Cells<100)
haem2<-filter(haem2, haem2$Total<1000)
haem2<-filter(haem2, haem2$BuffyCoat<5)
haem2$wbc_z<-scale(haem2$wbc)
haem2$Cells_z<-scale(haem2$Cells)

haem2<-haem2[,-c(1)]

#between
haem2 <- do.call("rbind", as.list(
  by(haem2, haem2["BirdID"], transform, AveAge=AveByInd(age))))


#within age centering 
haem2 <- do.call("rbind", as.list(
  by(haem2, haem2["BirdID"], transform, WithinAge=WithinIndCentr(age))))


haem2<-left_join(haem2, total_invert, by.x = c("FieldPeriodID","occasionyear"),all.x=T)
haem2<-left_join(haem2, tquali, by.x = c("TerritoryID", "FieldPeriodID","occasionyear"), all.x=T)
haem2$CatchTime<-str_sub(haem2$CatchTime, 12)
haem2$CatchTime<-chron(times=haem2$CatchTime)

haem2$newstat[haem2$Status %in% c("BrF", "BrM","BrU")]<-"Dom"
haem2$newstat[haem2$Status %in% c("ABX","TBrF","H","FLOAT","AB","SEEN1","OFL",
                              "SEEN2","B","FL","U")]<-"Sub"

#get observer 
obs<-sqlQuery(swdb, "SELECT tblCatches.CatchID, tblCatches.BirdID, tblOccasionIDs.OccasionID, tblOccasionIDs.Observer
FROM tblOccasionIDs INNER JOIN tblCatches ON tblOccasionIDs.OccasionID = tblCatches.OccasionID;
", stringsAsFactors=F)

haem2<-left_join(haem2, obs, by.x=c("OccasionID","BirdID"), all.x=T)
haem2<-haem2[-1,] #rm bird id 3 - bird from cousine


write.csv(haem2, "haematocrit_model.csv")




# plot(buffy,select=1,seWithMean=2,shade=TRUE,trans=exp,shift=coef(buffy)[1],main="Buffy Coat",
#      ylab="Buffy Coat",xlab="Age",xlim = c(0,20))
# 
# 
# 
# plot(buffy_old,select=1,seWithMean=2,shade=TRUE,trans=exp,shift=coef(buffy_old)[1],main="Buffy Coat",
#      ylab="Buffy Coat",xlab="Age",xlim = c(3,20))
# 
# 
# ggplot(haem2, aes(x=age, y=wbc_z))+geom_point(alpha=0.2)+geom_smooth(method="gam")
# 
# 
# 
# plot(cells,select=1,seWithMean=2,shade=TRUE,shift=coef(cells)[1],main="Haematocrit",
#      ylab="Haematocrit",xlab="Age",xlim = c(0,20))
# 
# ggplot(haem2, aes(x=age,y=Cells))+geom_point(alpha=0.2)+
#   geom_smooth(method="gam")+xlab("Age")+ylab("Haematocrit")
# 
# #removed trans=exp
# 
# ggplot(haem2, aes(x=age_date, y = Cells))+ geom_point()+
#   geom_smooth(method="gam")+xlab("Haematocrit")
# haemplot



#malaria model 

malaria<-read.csv("cleanmalaria.csv", stringsAsFactors = F, sep = ";")
malaria<-malaria[,-c(1)]
m_wb_age<-malaria


#needs fpid, insects, status, tq

#get FP, status 
fp_catch_occasion<-sqlQuery(swdb, "SELECT tblCatches.CatchID, tblCatches.BirdID, tblCatches.OccasionID, tblCatches.TerritoryID, sys_OccasionPlusFieldPeriod.FieldPeriodID, sys_OccasionPlusFieldPeriod.OccasionDate
FROM tblCatches INNER JOIN sys_OccasionPlusFieldPeriod ON tblCatches.OccasionID = sys_OccasionPlusFieldPeriod.OccasionID;
", stringsAsFactors=F)

fp_catch_occasion$OccasionDate<-as.Date(fp_catch_occasion$OccasionDate, "%Y-%m-%d")
m_wb_age$OccasionDate<-as.Date(m_wb_age$OccasionDate, "%Y-%m-%d")
fp_catch_occasion<-fp_catch_occasion[,c(1:5)]

m_wb_age<-left_join(m_wb_age, fp_catch_occasion, by.x = c('CatchID',"BirdID","OccasionDate"),all.x=T)
m_wb_age<-left_join(m_wb_age, total_invert, by.x=c("occasionyear","FieldPeriodID"),all.x=T)
m_wb_age<-left_join(m_wb_age, brgp_status_location, by.x=c("FieldPeriodID","BirdID"),all.x=T)



#status
m_wb_age$newstat[m_wb_age$Status %in% c("BrF", "BrM","BrU")]<-"Dom"
m_wb_age$newstat[m_wb_age$Status %!in% c("BrF", "BrM","BrU")]<-"Sub"

m_wb_age<-left_join(m_wb_age,tquali, by.x=c("TerritoryID", "FieldPeriodID"), all.x=T)
m_wb_age<-left_join(m_wb_age, lifespan, by.x="BirdID", all.x=T)

write.csv(m_wb_age, "malaria_model.csv")


#########################################################

#telomere length model 

telomeres<-read.csv("telomeres.csv", stringsAsFactors = F)
telomeres<-telomeres[,-c(1)]

# #btwn
# telomeres <- do.call("rbind", as.list(
#   by(telomeres, telomeres["BirdID"], transform, AveAge=AveByInd(age_year))))
# 
# 
# #within age centering 
# telomeres <- do.call("rbind", as.list(
#   by(telomeres, telomeres["BirdID"], transform, WithinAge=WithinIndCentr(age_year))))


telomeres<-merge(telomeres, birthfp, by.x = "BirdID", all.x = T)
telomeres<-merge(telomeres, sex, by.x = "BirdID", all.x = T)

#FPID,territory quality
telomeres<-left_join(telomeres,fp_catch_occasion, by.x=c("CatchID", "BirdID"),all.x=T)

telomeres<-left_join(telomeres,tquali, by.x=c("TerritoryID","FieldPeriodID"), all.x=T)

#status
telomeres$newstat[telomeres$Status %in% c("BrF", "BrM","BrU")]<-"Dom"
telomeres$newstat[telomeres$Status %in% c("ABX","TBrF","H","FLOAT","AB","SEEN1","OFL",
                                        "SEEN2","B","FL","U","NSA")]<-"Sub"

#yearly quality 
telomeres<-left_join(telomeres, total_invert, by.x=c("FieldPeriodID", "occasionyear"), all.x=T)


telomeres$RTL_z<-scale(telomeres$RTL)
# telomeres$avg_mass_z<-scale(telomeres$avg_mass)

telomeres<-left_join(telomeres, lifespan, by.x="BirdID", all.x=T)
write.csv(telomeres, "telomere_model.csv")







#survival
survival<-read.csv("survival.csv", stringsAsFactors = F, sep=',')

survival<-survival[,-c(1)]


survival$newstat[survival$Status %in% c("BrF", "BrM","BrU")]<-"Dom"
survival$newstat[survival$Status %in% c("ABX","TBrF","H","FLOAT","AB","SEEN1","OFL",
                                          "SEEN2","B","FL","U","NSA")]<-"Sub"
survival$newstat[survival$Status=="NS"]<-"dead"


#getting age of first reproduction because ars doesnt have all of them? 

firstrepro<-data.frame()

for(i in unique(survival$BirdID)){
   onebird<-survival%>%
     filter(BirdID==i)
   statuses<-filter(onebird, onebird$Status=="BrF"| onebird$Status=="BrM")
   if(nrow(statuses)==0){j<-data.frame(BirdID=i, agefirstrepro=NA)
   }else{j<-data.frame(BirdID=i, agefirstrepro=min(statuses$age, na.rm = T))
 }
   firstrepro<-rbind(j,firstrepro)
}
#age of first reproduction for survival data is not the same as 
#afr for ARS data. because survival first repro is based on first 
#dominant breeder status, while ARS is based on first age of having a chick
#plus first repro from survival data has more individuals than ARS 
#because ARS based on pedigree data 

#oh fuck it doesnt work like that because the fp will be okay ill just put year 

#based off ARS there will be 11498 NAs in the age first repro 

survivalsenescence<-merge(survival, firstrepro, by.x=c("BirdID"), all.x=T)

survivalsenescence<-merge(survivalsenescence, lifespan, by.x = "BirdID", all.x=T)

#with fps 
#assign summer and winter seasons 
fps<-as.data.frame(fps)
fps$PeriodEnd<-as.Date(fps$PeriodEnd, "%Y-%m-%d")
fps$PeriodStart<-as.Date(fps$PeriodStart, "%Y-%m-%d")
fps$seasonlength<-fps$PeriodEnd-fps$PeriodStart

fps<-fps%>%
  group_by(PeriodYear)%>%
  mutate(season=case_when(seasonlength == max(seasonlength)~"main",
                          seasonlength != max(seasonlength)~"winter"))

fps<-as.data.frame(fps)

survivalsenescence$occasionyear<-survivalsenescence$PeriodYear

survivalsenescence<-survivalsenescence[,-c(6,7)]
# survivalsenescence$PeriodStart<-as.Date(survivalsenescence$PeriodStart, "%d/%m/%Y")
# survivalsenescence$PeriodEnd<-as.Date(survivalsenescence$PeriodEnd, "%Y-%m-%d")

survivalsenescence<-left_join(survivalsenescence, fps, by.x = c("FieldPeriodID"), all.x=T)
#first get one status per year --take summer status 

test<-survivalsenescence
test<-test%>%group_by(BirdID, PeriodYear)%>%mutate(no_st=length(newstat))

# for(i in unique(test$BirdID)){
#   onebird<-filter(test, test$BirdID==i)
#   for(i in unique(onebird$PeriodYear)){
#     if(test$no_st==1){
#     }else{  filter(df, df$season=="main")}
#   }
# }

#if bird has more than one status that year, 
#take status of the fieldperiod that is summer 
morethan1<-test|>filter(no_st>1)
mainst<-morethan1|>filter(season=="main")

onest<-test|>filter(no_st==1)

ssdf<-rbind(mainst, onest)

morethan1<-morethan1%>%
  group_by(BirdID, PeriodYear)%>%
  mutate(s2=length(unique(season)))

oneseason<-morethan1%>%filter(s2==1)
oneseason<-oneseason[c(2,4,6,8),]
oneseason<-oneseason[,-c(22)]

ssdf<-rbind(ssdf,oneseason)

summary<-ssdf%>%
  group_by(BirdID)%>%
  summarize(years=length(unique(PeriodYear)))

summaryss<-survivalsenescence%>%
  group_by(BirdID)%>%
  summarize(years=length(unique(PeriodYear)))

s<-anti_join(summary,summaryss)


#retrieve status of previous year 
#for each bird
#for each period year it was seen 
#remove 

#assign field period to NA values 
ssdf$fp_na<-is.na(ssdf$FieldPeriodID)

fps<-fps%>%
  add_row(FieldPeriodID=-1, PeriodYear=1992)

for(i in 1:nrow(ssdf)){
  if(is.na(ssdf$FieldPeriodID[i])){
    periodyear<-ssdf$PeriodYear[i]
    fp<-fps[fps$season=="main"&fps$PeriodYear==periodyear,1]
    ssdf$FieldPeriodID[i]<-fp
    }
}



# survivalsenescence<-survivalsenescence[,-c(4,7)]

#get rid of the irrelevant stuff for yearly quality 
yearly_bug<-total_invert[,c("occasionyear","FieldPeriodID",'avg_invert')]
yearly_bug<-unique(yearly_bug)

yearly_bug<-yearly_bug%>%
  group_by(Year)%>%
  mutate(avg_bug=mean(avg_invert))
yearly_bug$occasionyear<-yearly_bug$Year
yearly_bug<-yearly_bug[,c(4,5)]
yearly_bug<-unique(yearly_bug)

ssdf<-as.data.frame(ssdf)
ssdf<-left_join(ssdf,yearly_bug, by.x=c("occasionyear"), all.x = T)

# survivalsenescence<-left_join(survivalsenescence, yearly_bug, by.x=c("occasionyear", "FieldPeriodID"), all.x=T)

#status of birds the year before 
stats<-ssdf[,c("BirdID","newstat","PeriodYear")]


# stats%>%
#   group_by(BirdID) %>% 
#   arrange(PeriodYear) %>% 
#   summarise()


#


#for each bird 
#i want to match period year with the year before 

stats <- stats[order(stats$BirdID, stats$PeriodYear), ]

# Create a new column 'statbefore' initialized with NA
stats$statbefore <- NA

# Iterate over each row in the dataframe
for (i in 2:nrow(stats)) {
  # Check if the current 'birdID' is the same as the previous row
  if (stats$BirdID[i] == stats$BirdID[i-1]) {
    # Assign the 'status' from the previous row to 'statbefore'
    stats$statbefore[i] <- stats$newstat[i-1]
  }
}

for(i in 1:nrow(stats)){
  if(is.na(stats$statbefore[i])){
    stats$statbefore[i]<-"Sub"
  }
}

#if status was NA make them NA as well 
for(i in 1:nrow(stats)){
  if(is.na(stats$newstat[i])){
    stats$statbefore[i]<-NA
  }
}


#
ssdf<-left_join(ssdf, stats, by.x=c("BirdID","PeriodYear","newstat"),all.x=T)

###




#territory quality stuff 
repter<-status_terr_brgp%>%
  filter(gpno>1)

length(unique(repter$BirdID))
#273 birds sighted in more than 1 territory in 1 fs 

# fpyr<-survivalsenescence[,c("FieldPeriodID", "PeriodYear")]
# fpyr<-unique(fpyr)
# fpyr<-na.omit(fpyr)
status_terr_brgp<-left_join(status_terr_brgp, fps, by.x="FieldPeriodID", all.x=T)

tq_stb<-left_join(status_terr_brgp, tquali, by.x="FieldPeriodID", all.x=T)

birbs<-unique(ssdf$BirdID)
tq_stb<-tq_stb%>%
  filter(BirdID %in% birbs)

tq_stb2<-tq_stb%>%
  group_by(BirdID, PeriodYear)%>%
  mutate(yearlytq=mean(TQ, na.rm = T),  #something is very wrong with the mean function
         yearlytq_corr=mean(TQcorrected, na.rm=T))

#birdterritory<-brgp_status_location[,c(2:5)]
#test<-left_join(survivalsenescence, birdterritory, by.x=c("BirdID", "FieldPeriodID"), all.x=T)
#TEST 
yearlytq<-tq_stb2[,c("BirdID",'FieldPeriodID',"PeriodYear","yearlytq","yearlytq_corr","Year")]
yearlytq<-unique(yearlytq)

ssdf<-left_join(ssdf, yearlytq, by.x=c("BirdID","PeriodYear"),all.x=T)

# survivalsenescence<-left_join(survivalsenescence, tquali)

write.csv(ssdf, "survival_model.csv")





df<-data.frame(age=physio$age_year)
df$scale_i<-scale(I(df$age))
df$scale<-scale(df$age)
df$age2<-df$age^2
df$scale2<-scale(df$age2)
df$scale3<-scale(df$age)^2
df$scale_i2<-scale(I(df$age2))
