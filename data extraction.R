# install.packages("RODBC")
# install.packages("data.table")
# install.packages("Hmisc")
library(data.table)
library(ggplot2)
library(Hmisc)   #has %nin%
library(RODBC)

setwd("C:/Database")
"%!in%" <-Negate("%in%")
#mismatched<-Negate('intersect')


#connecting the database

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:/db2/SeychellesWarbler1.11beta.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)


#fetching data...

birdid<-sqlFetch(swdb, "tblBirdID", stringsAsFactors=F)  #bird id

#colourring<-sqlFetch(swdb, "sys_CurrentFieldRing", stringsAsFactors=F) #colour rings




#ages copied from the other script
setwd("C:/PhD/Data")
lastseen<-read.csv("lastseen.csv", header = T, sep = ";")  #is status by field period query
pedigree<-read.csv('updated pedigree.csv', sep=";")
birdid<-read.csv("tblBirdID.csv", sep = ";")

library("tidyverse")
lastseen$birthyear<-as.numeric(substr(lastseen$BirthDate, 7,10))
lastseen$PeriodYearEnd<-as.numeric(str_sub(lastseen$PeriodEnd, 7,10))
lastseen$birthday<-as.Date(lastseen$BirthDate, "%d/%m/%Y")
#lastseen$age<-lastseen$PeriodYearEnd-lastseen$birthyear

lastseen<-filter(lastseen,lastseen$Island=="CN")
lastseen<-filter(lastseen,lastseen$Status!="NS")
names(lastseen)[names(lastseen) == 'sys_StatusByFieldPeriod_FieldPeriodID'] <- 'FieldPeriodID'
lastseen<-lastseen[,-c(4)]

#occasion ID to field period ID
FP_occasion<-sqlFetch(swdb, "sys_OccasionPlusFieldPeriod", stringsAsFactors=F)
FP_occasion<-FP_occasion[,-c(6,7)]
FP_occasion<-filter(FP_occasion, FP_occasion$Island=="CN")


#get observer
occasion<-sqlFetch(swdb, "tblOccasionIDs", stringsAsFactors=F)
observer<-occasion[,c(1:4)]
observer<-filter(observer, observer$Island=="CN")

#getting body mass, LR tarsus, wing, head, bill
catch<-sqlQuery(swdb, c("SELECT tblCatches.*, tblOccasionIDs.OccasionDate, tblOccasionIDs.Island
                            FROM tblOccasionIDs INNER JOIN tblCatches ON tblOccasionIDs.OccasionID = tblCatches.OccasionID;"),
                 stringsAsFactors=F )

#trick for using sqlQuery -> make a query in access, make sure it works and copy and paste into a c(" ")
#use right tars

physio<-catch[,c(1:5,8,15:21,24:27,38,39)]
physio$CatchTime<-str_sub(physio$CatchTime,11)
physio$occasionyear<-as.numeric(str_sub(physio$OccasionDate,1,4))
birthyear<-lastseen[,c("BirdID","birthyear","birthday")]
birthyear<-unique(birthyear)

statusperyear<-lastseen[,c("BirdID","FieldPeriodID","Status","birthyear","PeriodYearEnd")]
statusperyear<-unique(statusperyear)
#match bird id, merge, occasion year - birth year is age.

physio<-left_join(x = physio, y = birthyear, by.x = "BirdID", all.x = T)

physio$OccasionDate<-as.Date(physio$OccasionDate, "%Y-%m-%d")
physio$age_date<-physio$OccasionDate-physio$birthday
physio$age_date<-as.numeric(physio$age_date)

physio$age_year<-physio$occasionyear-physio$birthyear

physio<-filter(physio, physio$BirdID>0)
physio<-filter(physio, physio$Island=="CN")

#get observer for phys traits

#link occasion to field period
physio<-merge(physio, FP_occasion, by.x=c("OccasionID","OccasionDate","Island"), all.x=T)

#then get the observer
physio<-merge(physio, statusperyear, by.x = c('BirdID',"birthyear","FieldPeriodID"), all.x = T)

#remove chicks and na
remove<-c("CH","FL","",NA)
physio<-filter(physio, physio$Status %!in% remove)

#save csv
write.csv(physio, "physiological.csv")


#fuck i couldve done this so much easier with sys_CatchPlusStatus thats so stupid ugh
#centre the variables. (z transform)


#reproductive traits

#1. ars. genetic parents 80% confidence level
sex<- read.csv("sys_SexEstimates.csv", sep=";")  #cant pull from db for some reason
sex<-sex[,1:2]  #0 f 1 M

pedigree <- merge(pedigree,sex, by = "BirdID", all.x = T)


#pedigree$BirdID<-as.character(pedigree$BirdID)
pedigree2<-filter(pedigree, pedigree$GenDadConfidence>=80 & pedigree$GenMumConfidence>=80)

pedigree2<-filter(pedigree, pedigree$GenDadConfidence>=80 |pedigree$GenMumConfidence>=80)

#link birthday to fieldperiod
#get birth period
birthfp<-sqlQuery(swdb, "SELECT tblBirdID.BirdID, tblBirdID.BirthFieldPeriodID
FROM tblBirdID;", stringsAsFactors=F)


#merge
pedigree2<-left_join(pedigree2, birthfp, by.x=c("BirdID","BirthDate"), all.x = T)

pedigree2<-pedigree2[,-c(4,5)]

pedigree2$chickyear<-as.numeric(str_sub(pedigree2$BirthDate, 7))

#annual reproductive success

 dams<-list()
 for(i in pedigree2$BirdID){
   dam<-data.frame()
   dam<-filter(pedigree2, pedigree2$GeneticMother==i)
   dams[[i]]<-dam
 }

 sires<-list()
 for(i in pedigree2$BirdID){
   sire<-data.frame()
   sire<-filter(pedigree2, pedigree2$GeneticFather==i)
   sires[[i]]<-sire
 }


 dams<-do.call(rbind.data.frame, dams)
 sires<-do.call(rbind.data.frame, sires)

 sires$chickyear<-str_sub(sires$BirthDate, 7,10)


 dars<-data.frame()
 dars<-dams%>%
 group_by(GeneticMother, chickyear)%>%
 mutate(ars=length(chickyear))  #originally this row is summarize to make new df



 dars<-as.data.frame(dars)
 names(dars)[names(dars) == 'BirdID'] <- 'ChickBirdID'
 names(dars)[names(dars) == 'GeneticMother'] <- 'BirdID'
 dars<-dars[,c("NestID","BirdID","chickyear","ars")]


 sars<-data.frame()
 sars<-sires%>%
   group_by(GeneticFather, chickyear)%>%
   mutate(ars=length(chickyear))  #originally this row is summarize to make new df



 sars<-as.data.frame(sars)
 names(sars)[names(sars) == 'BirdID'] <- 'ChickBirdID'
 names(sars)[names(sars) == 'GeneticFather'] <- 'BirdID'
 sars<-sars[,c("BirdID","chickyear","ars","NestID")]


 #fill in the gaps where they werent reproducing

 #mom
 d<-subset(lastseen, lastseen$BirdID %in% dars$BirdID) #gets genetic mom
 d<-d[,c("BirdID","PeriodYear")]
 d$chickyear<-d$PeriodYear
 d<-unique(d)

 idd<-data.frame()
 #i<-444
 for(i in unique(d$BirdID)){
   bird<-filter(d, d$BirdID==i)
   birds<-filter(dars, dars$BirdID==i)
   no<-subset(bird, bird$chickyear %!in% birds$chickyear)
   no<-unique(no)
   idd<-rbind(no,idd)
 }
 idd$ars<-0

 # names(idd)[names(idd) == 'PeriodYear'] <- 'chickyear'
 # names(idd)[names(idd) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'

 idd<-idd%>%
   mutate(NestID=NA)
  idd<-idd[,-c(2)]


 #dad turn
 s<-subset(lastseen, lastseen$BirdID %in% sars$BirdID) #gets genetic dad
 s<-s[,c("BirdID","PeriodYear")]
 s$chickyear<-s$PeriodYear
 s<-unique(s)

 sars<-as.data.frame(sars)

 ids<-data.frame()
 for(i in unique(s$BirdID)){
   bird<-filter(s, s$BirdID %in% i)
   birds<-filter(sars, sars$BirdID %in% i)
   no<-subset(bird, bird$chickyear %!in% birds$chickyear)
   no<-unique(no)
   ids<-rbind(no,ids)
 }
 ids$ars<-0
 # names(ids)[names(ids) == 'PeriodYear'] <- 'chickyear'
 # ids<-ids[,-c(2)]

 ids<-ids%>%
   mutate(NestID=NA,)
 ids<-ids[,-c(2)]


 Female_rs<-rbind(idd,dars)
 Malears<-rbind(ids,sars)

 Female_rs$sex<-0
 Malears$sex<-1
 annualReprosuccess<-rbind(Female_rs, Malears)

 #adding other stuff
 annualReprosuccess<-left_join(annualReprosuccess,birthyear,by.x="BirdID", all.x = T)
 annualReprosuccess$chickyear<-as.numeric(annualReprosuccess$chickyear)
 annualReprosuccess$parent_age<-annualReprosuccess$chickyear-annualReprosuccess$birthyear

 annualReprosuccess<-unique(annualReprosuccess)
 #some had age -1 chick was born before parent
 #assigned age 0
 annualReprosuccess$parent_age[annualReprosuccess$parent_age== -1]<-0

 write.csv(annualReprosuccess, "annualreprosuccess.csv")


 #########################################
 #field period
drs_fp<-data.frame()
drs_fp<-pedigree2%>%
  group_by(GeneticMother, BirthFieldPeriodID)%>%
  mutate(rs=length(BirthFieldPeriodID))

# drs_fp<-drs_fp%>%
#   group_by(GeneticMother, chickyear)|>
#   mutate(ars=length(chickyear))

# drs_fp$m<-drs_fp$ars==drs_fp$rs

drs_fp<-as.data.frame(drs_fp)
names(drs_fp)[names(drs_fp) == 'BirdID'] <- 'ChickBirdID'
names(drs_fp)[names(drs_fp) == 'GeneticMother'] <- 'BirdID'
drs_fp<-drs_fp[,c("ChickBirdID","BirthDate","NestID","BirdID","BirthFieldPeriodID","rs")]
drs_fp$chickyear<-str_sub(drs_fp$BirthDate, 7,10)


sARS<-data.frame()
# sARS<-pedigree2%>%
#   group_by(GeneticFather, chickyear)%>%
#   mutate(s_ars=length(chickyear))
sARS<-pedigree2%>%
  group_by(GeneticFather, BirthFieldPeriodID)%>%
  mutate(s_rs=length(BirthFieldPeriodID))

sARS<-as.data.frame(sARS)
names(sARS)[names(sARS) == 'BirdID'] <- 'ChickBirdID'
names(sARS)[names(sARS) == 'GeneticFather'] <- 'BirdID'
sARS<-sARS[,c("ChickBirdID","BirthDate","NestID","BirdID","BirthFieldPeriodID", "chickyear","s_rs")]

# reproductive success by field periods
fps<-readxl::read_excel("fps.xlsx")
d<-subset(lastseen, lastseen$BirdID %in% drs_fp$BirdID) #gets genetic mom
d<-d[,c("BirdID","PeriodYear")]
d<-merge(d,fps, by.x="PeriodYear",all.x=T)
d<-unique(d)

idd<-data.frame()
#i<-444
for(i in unique(d$BirdID)){
  bird<-filter(d, d$BirdID==i)
  birds<-filter(drs_fp, drs_fp$BirdID==i)
  no<-subset(bird, bird$FieldPeriodID %!in% birds$BirthFieldPeriodID)
  no<-unique(no)
  idd<-rbind(no,idd)
}
idd$rs<-0
# idd$ars<-NA

names(idd)[names(idd) == 'PeriodYear'] <- 'chickyear'
names(idd)[names(idd) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'

idd<-idd%>%
  mutate(ChickBirdID=NA, BirthDate=NA,NestID=NA,)
idd<-idd[,-c(4:6)]



s<-subset(lastseen, lastseen$BirdID %in% sARS$BirdID) #gets genetic dad
s<-s[,c("BirdID","PeriodYear")]
s<-merge(s,fps, by.x="PeriodYear",all.x=T)
s<-unique(s)

#find the years where they had no reproductive success


ids<-data.frame()

for(i in unique(s$BirdID)){
  bird<-filter(s, s$BirdID==i)
  birds<-filter(sARS, sARS$BirdID==i)
  no<-subset(bird, bird$FieldPeriodID %!in% birds$BirthFieldPeriodID)
  no<-unique(no)
  ids<-rbind(no,ids)
}
ids$s_rs<-0
names(ids)[names(ids) == 'PeriodYear'] <- 'chickyear'
names(ids)[names(ids) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'
ids<-ids[,-c(4:6)]

ids<-ids%>%
  mutate(ChickBirdID=NA, BirthDate=NA,NestID=NA,)
# ids$ars<-NA

Female_rs<-rbind(idd,drs_fp)
Malears<-rbind(ids,sARS)

Female_rs$sex<-0
Malears$sex<-1
names(Malears)[names(Malears)=="s_rs"]<-"rs"
Reprosuccess<-rbind(Female_rs, Malears)



#redo
Reprosuccess<-left_join(Reprosuccess,birthyear,by.x="BirdID", all.x = T)
Reprosuccess$chickyear<-as.numeric(Reprosuccess$chickyear)
Reprosuccess$parent_age<-Reprosuccess$chickyear-Reprosuccess$birthyear

Reprosuccess<-Reprosuccess[,-c(5:7)]
Reprosuccess<-unique(Reprosuccess)
#some had age -1 chick was born before parent
#assigned age 0
Reprosuccess$parent_age[Reprosuccess$parent_age== -1]<-0

write.csv(Reprosuccess, "reprosuccess.csv")

#no longer need this
# ARS<-filter(ARS, ARS$chickyear<=2017) #remove anything past summer 2017
# ARS<-filter(ARS, ARS$parent_age>=0)

somethingisfishy<-ARS[,c("BirdID","chickyear",'ars','sex','birthyear','birthday','parent_age')]
xyz<-duplicated(somethingisfishy)
xyz<-somethingisfishy[xyz,]
#okay theres extra rows because of the chicks--one row per chick
#i really forgot why i was doing all this

# statusperyear$chickyear<-statusperyear$PeriodYearEnd
# ARS_status<-merge(ARS, statusperyear, by.x = c("BirdID","chickyear","birthyear"), all.x = T)

#i dont think I can do age with days for this one


#save the data into csv to load directly into next script
write.csv2(ARS, "ars.csv")
write.csv2(somethingisfishy, "checkarsdiff.csv")


#provisioning
nestwatch<-sqlQuery(swdb, "SELECT tblNestWatchBehaviours.*, tblNestWatchDetails.NestID, tblNestInfo.LayDateEarliest,
tblNestInfo.LayDateLatest, tblNestInfo.HatchDateEarliest, tblNestInfo.HatchDateLatest, tblNestWatchDetails.WatchDate,
tblNestInfo.BreedGroupID, tblNestInfo.ClutchSize, tblNestInfo.ClutchSizeEstimated, tblNestInfo.BroodSize, tblNestInfo.BroodSizeEstimated, tblNestWatchDetails.ObsDuration, tblNestWatchDetails.StartTime, tblNestWatchDetails.Accurate
FROM (tblNestInfo INNER JOIN tblNestWatchDetails ON tblNestInfo.NestID = tblNestWatchDetails.NestID) INNER JOIN tblNestWatchBehaviours ON tblNestWatchDetails.NestWatchID = tblNestWatchBehaviours.NestWatchID;", stringsAsFactors=F)


provisioning<-filter(nestwatch, nestwatch$Behaviour=="F")
provisioning<-filter(provisioning, provisioning$Accurate==1)

provisioning<-provisioning[,-c(1,9:11)]
provisioning$Start<-str_sub(provisioning$Start,12)
provisioning$Stop<-str_sub(provisioning$Stop,12)

provisioning<-filter(provisioning, provisioning$ObsDuration>=60)

#provisioning<-filter(provisioning, provisioning$BroodSize==1)  # do i have to do this?
provisioning<-unique(provisioning)

provisioning<-filter(provisioning, provisioning$BirdID>0)



breedgroupinfo<-sqlFetch(swdb, "tblBreedStatus", stringsAsFactors=F)

#breedgroupinfo<-filter(breedgroupinfo, breedgroupinfo$Status=="U")
breedgroupinfo<-breedgroupinfo[,-c(6:8)]
breedgroupinfo<-filter(breedgroupinfo, breedgroupinfo$BirdID>0)

#birds in breed group with multiple breed status??

provisioning<-left_join(provisioning, breedgroupinfo, by=c("BirdID", "BreedGroupID"))
provisioning$StartTime<-str_sub(provisioning$StartTime, 12)

#group by nestwatch ID and obs start and count number of starts
provisioning<- provisioning%>%
  group_by(NestWatchID, StartTime,BirdID)%>%
  mutate(nrfeeding=length(unique(Start)))
provisioning<-as.data.frame(provisioning)

provisioning$prate<-provisioning$nrfeeding/provisioning$ObsDuration
provisioning$year<-str_sub(provisioning$WatchDate,1,4)


provi<-provisioning[,c("NestWatchID","BirdID","Behaviour","NestID",
                       "LayDateEarliest","LayDateLatest","HatchDateEarliest",
                       "HatchDateLatest","WatchDate","BreedGroupID",
                       "ClutchSize","BroodSize","ObsDuration",
                       "StartTime","StatusID","Status","Observer",
                       "year","nrfeeding","prate")]

provi<-unique(provi)
provi<-merge(provi, birthyear, all.x = T)
provi$year<-as.numeric(provi$year)
provi$age_years<-provi$year-provi$birthyear
provi$WatchDate<-as.Date(provi$WatchDate, "%Y-%m-%d")
provi$age_days<-provi$WatchDate-provi$birthday
provi$age_days<-as.numeric(provi$age_days)

#save data
write.csv(provi, "provisioning.csv")

# dim(merge(provisioning,age, by.x = c("BirdID", "year"), all.x = T))

#no of times feeding of 1 bird over observation time provision rate

#need to control for brood size when doing the age effect


#filter out those that have more than 1 chick


#personality
Personality<-sqlFetch(swdb, "tblPersonality", stringsAsFactors=F)
Personality<-Personality[,-c(43,44)]


catchandoccasion<-sqlQuery(swdb, "SELECT tblCatches.CatchID, tblCatches.OccasionID, tblOccasionIDs.Island, tblOccasionIDs.OccasionDate, tblCatches.BirdID
FROM tblOccasionIDs INNER JOIN tblCatches ON tblOccasionIDs.OccasionID = tblCatches.OccasionID;", stringsAsFactors=F)

Personality<-left_join(Personality, catchandoccasion, by.x ="CatchID", all.x = T)

Personality<-filter(Personality, Personality$Observer!="AAS") #remove alexanders observations

Personality<-merge(Personality, birthyear, by.x = "BirdID", all.x = T)

Personality<-Personality%>%
  group_by(BirdID)%>%
  mutate(repeats=length(CatchID))

Personality<-as.data.frame(Personality)

reasonable<-filter(Personality, Personality$repeats>3)
length(unique(reasonable$BirdID))

#24 birds with more than 3 repeats of personality data and


#fighting
obs<-sqlFetch(swdb, "tblObservationActivity", stringsAsFactors=F)

fighting<-filter(obs, obs$Activity=="FT")


  #add the birth date of the chick into the df
#ind

#blood info
bloodinfo<-sqlQuery(swdb,"SELECT tblBloodSampleDetails.BloodID, tblBloodSampleDetails.BirdID, tblBloodSampleDetails.CatchID, tblBloodSampleDetails.BloodSource, tblOccasionIDs.OccasionDate, tblOccasionIDs.Island
FROM tblOccasionIDs INNER JOIN (tblCatches INNER JOIN tblBloodSampleDetails ON tblCatches.CatchID = tblBloodSampleDetails.CatchID) ON tblOccasionIDs.OccasionID = tblCatches.OccasionID;
", stringsAsFactors=F )
bloodinfo$occasionyear<-as.numeric(str_sub(bloodinfo$OccasionDate,1,4))


#telomeres, haematocrit, oxidative stress
telomeres<-read.csv("REL_TL_EB_EAF_24 May.csv", stringsAsFactors = F)

telomeres <- subset(telomeres,CqTelomere < 25) %>%
  subset(CqGAPDH < 26) %>%
  subset(CqGAPDH > 21) %>%
  subset(DiffGAPDH < 0.5) %>%
  subset(DiffTelomere < 0.5) %>%
  subset(RTL < 3) %>%
  subset(BloodID>6)

#control for plate and technician
#duplicate blood ID -different telomere, take EAF when both technicians
#if multiple then erm


# dup<-data.frame()
# for(i in unique(telomeres$BloodID)){
#   row<-nrow(filter(telomeres, telomeres$BloodID==i))
#   row1<-data.frame(i,row)
#   dup<-rbind(dup,row1)
# }


telomeres$BloodID<-as.character(telomeres$BloodID)

telomeres2<-data.frame()
telomeres2<-telomeres%>%
  dplyr::group_by(BloodID)%>%
  dplyr::mutate(obsno=length(unique(Whodunnit)))


#duplicated blood ids
#check how many observers
#if more than 1 obs remove EB from telomeres
telomeres2$BloodID<-as.numeric(telomeres2$BloodID)
telomeres2<-as.data.frame(telomeres2)

#remove EB if multiple blood id
cleantelo<-data.frame()
for(i in unique(telomeres2$BloodID)){
  oneid<-filter(telomeres2, telomeres2$BloodID==i)
  obsno<-oneid$obsno[1]
  if(obsno==1){
    cleantelo<-rbind(oneid,cleantelo)
  }else{wanted<-oneid[!(oneid$Whodunnit=="EB" & oneid$BloodID==i),]
    cleantelo<-rbind(cleantelo,wanted)
  }
}


#taking a random measurement from the data
telos<-cleantelo%>%
  group_by(BloodID, Whodunnit)%>%
  sample_n(size = 1, replace = F)


#link age and status
bloodid_status<-sqlQuery(swdb,"SELECT tblBloodSampleDetails.BloodID, tblBloodSampleDetails.BirdID, tblBloodSampleDetails.CatchID, tblBloodSampleDetails.BloodSource, sys_CatchPlusStatus.Status, sys_CatchPlusStatus.OccasionDate, tblOccasionIDs.Island
FROM (tblBloodSampleDetails INNER JOIN sys_CatchPlusStatus ON (tblBloodSampleDetails.CatchID = sys_CatchPlusStatus.CatchID) AND (tblBloodSampleDetails.BirdID = sys_CatchPlusStatus.BirdID)) INNER JOIN tblOccasionIDs ON sys_CatchPlusStatus.OccasionID = tblOccasionIDs.OccasionID;
", stringsAsFactors=F)

#get rid of chicks
chicks<-c("CH","FL","EGG")
bloodid_status<-filter(bloodid_status, bloodid_status$Status %!in% chicks)

telos<-merge(telos, bloodid_status, by.x = "BloodID", all.x = T)

#age
telos<-merge(telos,birthyear, by.x = "BirdID", all.x = T)

telos$occasionyear<-as.numeric(str_sub(telos$OccasionDate,1,4))
telos$OccasionDate<-as.Date(telos$OccasionDate, "%Y-%m-%d")

telos$age_year<-telos$occasionyear-telos$birthyear
telos$age_date<-telos$OccasionDate-telos$birthday
telos<-filter(telos, telos$Island=="CN")  #cousin only

write.csv(telos, "telomeres.csv")

#haematocrit
haematocrit<-sqlFetch(swdb, "tblHaematocrit", stringsAsFactors=F)
haematocrit<-haematocrit[,-c(7,9,10)]
haematocrit<-merge(haematocrit, birthyear, by="BirdID", all.x = T)

#some birds have multiple blood IDs
bloodhaematocrit<-filter(bloodinfo, bloodinfo$Island=="CN")
bloodhaematocrit<-bloodhaematocrit%>%
  group_by(BirdID, CatchID)%>%
  mutate(oc=length(OccasionDate), year= length(occasionyear))

wantedbloodid<-subset(bloodhaematocrit,bloodhaematocrit$oc==1)
duplicatebloodID<-subset(bloodhaematocrit,bloodhaematocrit$oc==2)
wantedbloodid<-as.data.frame(wantedbloodid)
haematocrit<-merge(haematocrit, wantedbloodid, by.x=c("BirdID","CatchID"), all.x = T)


haematocrit$age<-haematocrit$occasionyear-haematocrit$birthyear

write.csv(haematocrit, "haematocrit.csv")


#oxidative stress

oxidativestress<-sqlFetch(swdb, "tblOxidativeStress", stringsAsFactors=F)

oxidativestress<-oxidativestress[,-c(1,6,16,24,25,27:29)]

oxidativestress<-filter(oxidativestress, oxidativestress$UseData==1)


oxidativestress<-merge(oxidativestress, bloodinfo, by.x = "BloodID", all.x = T)


#names(age)[names(age) == 'PeriodYear'] <- 'year'

oxidativestress<-merge(oxidativestress, birthyear, by.x = c("BirdID"), all.x = T)

oxidativestress<-unique(oxidativestress)

oxidativestress<-filter(oxidativestress, oxidativestress$Island=="CN")

oxidativestress$age_year<-oxidativestress$occasionyear-oxidativestress$birthyear
oxidativestress$OccasionDate<-as.Date(oxidativestress$OccasionDate, "%Y-%m-%d")
oxidativestress$age_date<-oxidativestress$OccasionDate-oxidativestress$birthday


write.csv(oxidativestress, "oxidativestress.csv")


#malaria data

malaria<- sqlFetch(swdb,"tblMalariaTests", stringsAsFactors=F)
mconsensus<-sqlFetch(swdb, "sys_MalariaConsensus", stringsAsFactors=F)
malaria<-malaria[,-c(7:9)]

malaria<-merge(malaria, bloodinfo, by.x = "BloodID", all.x = T)
malaria<-merge(malaria,birthyear, by.x = "BirdID", all.x = T)
malaria$age<-malaria$occasionyear-malaria$birthyear

#write.csv(malaria, "malaria.csv")

#further cleaning
# malaria<-read.csv("malaria.csv", stringsAsFactors = F)
# malaria<-malaria[,-c(1)]

### filter dataset
#some tests with the same blood ID and bird ID have multiple results
malaria<-filter(malaria, malaria$Island=="CN")
malaria<-filter(malaria, malaria$BirdID>0)
malaria$TestResult[malaria$TestResult == 2] <- 1

#find the tests that went wrong
weirdresults<-malaria %>%
  group_by(BirdID, BloodID)%>%
  summarise(result=length(unique(TestResult)))

weirdresults<-as.data.frame(weirdresults)
wrongtests<-filter(weirdresults, weirdresults$result>1)
wrongtests<-wrongtests[,"BloodID"]

mcheck<-merge(malaria, mconsensus, by.x = "BloodID", all.x=T)

mcheck$check<-mcheck$TestResult==mcheck$MalariaConsensus

mcheck<-filter(mcheck, mcheck$check==TRUE)
mcheck<-left_join(mcheck, sex, by.x="BirdID", all.x=T)
mcheck<-mcheck[,-c(5)]
mcheck<-mcheck[,-c(3)]
mcheck<-unique(mcheck)


write.csv2(mcheck, "cleanmalaria.csv")

#2. offspring reproductive success?
#possibly brood size, but only around 13% have 2 or more eggs?
#does breeding status count



#survival
brstatus<-read.csv("lastseen.csv", stringsAsFactors = F, sep = ";")
brstatus<-filter(brstatus, brstatus$Island=="CN")

brstatus$birthyear<-as.numeric(substr(brstatus$BirthDate, 7,10))
brstatus$PeriodYearEnd<-as.numeric(str_sub(brstatus$PeriodEnd, 7,10))
brstatus$age<-brstatus$PeriodYearEnd-brstatus$birthyear
brstatus$BirthDate<-as.Date(brstatus$BirthDate,"%d/%m/%Y")
brstatus$PeriodEnd<-as.Date(brstatus$PeriodEnd, "%d/%m/%Y")
brstatus$age_date<-brstatus$PeriodEnd-brstatus$BirthDate

brstatus$survival<-1

brstatus$survival[brstatus$Status=="NS"]<-0


survival19<-filter(brstatus, brstatus$PeriodYearEnd<=2019)



alivebirds<-brstatus[(brstatus$Status!="NS" & brstatus$PeriodEnd>="2019-09-23"),]


survivalanalysis<-filter(survival19, survival19$BirdID %!in% alivebirds$BirdID)
#removestatus<-c("TBrM","","TBrF")
#survivalanalysis<-filter(survivalanalysis, survivalanalysis$Status %!in% removestatus)

lifespan<-survivalanalysis%>%
  group_by(BirdID)%>%
  summarise(lifespan=max(age_date))


survivalanalysis2<-survivalanalysis


#i<-20
for(i in unique(survivalanalysis2$BirdID)){
  onebird<-filter(survivalanalysis2, survivalanalysis2$BirdID==i)
  if(all(onebird$survival!=0)){
    lastyear<-max(onebird$PeriodYearEnd)#find the last year vector
    PeriodYear<-max(onebird$PeriodYear)
    age<-max(onebird$age)
    birthyear<-unique(onebird$birthyear)
    survivalanalysis2<-survivalanalysis2%>%
      add_row(BirdID=i, PeriodYearEnd=lastyear+1, PeriodYear=PeriodYear+1, Status="NS", age=age+1, survival=0, birthyear=birthyear)
    #and then add a new row into the dataframe and assign the survival as 0
  }
}

survivalanalysis2<-left_join(survivalanalysis2,sex,by.x="BirdID",all.x=T)

#should i remove chicks and fl from analysis
#remove translocated

translocated<-sqlFetch(swdb, "tblTranslocations", stringsAsFactor=F)

survivalanalysis2<-filter(survivalanalysis2, survivalanalysis2$BirdID %!in% translocated$BirdID)
# survivalanalysis<-filter(survivalanalysis, survivalanalysis$BirdID %!in% translocated$BirdID)



# ggplot(survivalanalysis2, aes(x=age,fill=as.factor(survival)))+geom_bar(position="stack")


# write.csv(survivalanalysis, "survivalanalysis.csv")
write.csv(survivalanalysis2, "survival.csv")

survival<-read.csv("survival.csv", stringsAsFactors = F)

lifespan<-survival%>%
  group_by(BirdID)%>%
  summarise(lifespan=max(age_date, na.rm = T))
length(unique(survival$BirdID))

write.csv2(lifespan, "maxlifespan.csv", row.names = F)

#not all dead birds have NS behind them
#check 2019-if birds have NS, include them in the analysis.
#everyone else can fuckoff
#
#
# alive22<-filter(brstatus, brstatus$PeriodYearEnd==2022 & brstatus$Status!="NS")
# alive22<-unique(alive22$BirdID)
#
# alive19<-filter(brstatus, brstatus$PeriodYearEnd==2019 & brstatus$Status!="NS")
# alive19<-unique(alive19$BirdID)
#
# alive<-c(alive22,alive19)
# alive<-unique(alive)
#
#
# survival<-filter(survival19, survival19$BirdID %!in% alive)
#
# survival<-as.data.frame(survival)

#something is wrong here

i<-6750
maxstatus<-data.frame()
for(i in unique(brstatus$BirdID)){
  onebird<-filter(brstatus, brstatus$BirdID==i)
  maxalive<-filter(onebird, onebird$PeriodEnd==max(onebird$PeriodEnd))
  maxalive<-maxalive[,c("BirdID","Status","PeriodEnd")]
  maxstatus<-rbind(maxalive,maxstatus)
  }




brstatus[max(brstatus$PeriodEnd),]

brstatus2<-brstatus

for(i in unique(brstatus$BirdID)){
  onebird<-filter(brstatus, brstatus$BirdID==i)
  if(all(onebird$survival!=0)){
    lastyear<-max(onebird$PeriodYearEnd)#find the last year vector
    age<-max(onebird$age)
    brstatus2<-brstatus2%>%
      add_row(BirdID=i, PeriodYearEnd=lastyear+1, Status="dead", age=age+1, survival=0)
    #and then add a new row into the dataframe and assign the survival as 0
  }
}

# names(brstatus2)[names(brstatus2) == 'sys_StatusByFieldPeriod_FieldPeriodID'] <- 'FieldPeriodID'
#
# new<-filter(brstatus2, brstatus2$Status=="dead")
# new<-new[,c("BirdID", "PeriodYearEnd", "Status", "survival")]
# new<-merge(new,birthyear, by.x = "BirdID", all.x = T)

#remove all the birds that are still alive
# alive<-c()
# for(i in unique(brstatus$BirdID)){
#   onebird<-filter(brstatus, brstatus$BirdID==i)
#   if(all(onebird$survival)==2){
#     alive<-c(i,alive)}
# }





















#RUBBISH CODE
# age<-lastseen[,c(1,9,11,12)]
# age$chickyear<-age$PeriodYear
# names(age)[names(age) == 'PeriodYear'] <- 'year'
# age<-unique(age)
#
# age<-as.data.frame(age)
#
# age2<-age%>%
#   group_by(BirdID, year)%>%
#   mutate(count=length(year))

