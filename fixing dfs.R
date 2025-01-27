setwd("C:/PhD/Data")
library(data.table)
library(ggplot2)
library(Hmisc)   #has %nin%
library(RODBC)
library(tidyverse)

"%!in%" <-Negate('%in%')
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:/db2/SeychellesWarbler1.11beta.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

maxlifespan<-read.csv("maxlifespan.csv",sep = ";")



lastseen<-sqlQuery(swdb, "SELECT sys_StatusByFieldPeriod.BirdID, sys_StatusByFieldPeriod.FieldPeriodID, sys_StatusByFieldPeriod.Status, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.PeriodYear
FROM sys_StatusByFieldPeriod INNER JOIN tblFieldPeriodIDs ON sys_StatusByFieldPeriod.FieldPeriodID = tblFieldPeriodIDs.FieldPeriodID;
", stringsAsFactors=F)

birdid<-sqlFetch(swdb, "tblBirdID", stringsAsFactors=F)
birdid<-birdid%>%
  filter(BirdID>0)%>%
  select(BirdID, BirthDate)


## new lifespan data ##
lastseen<-lastseen%>%filter(Island=="CN")


# lastseen$survival<-1
# lastseen$survival[lastseen$Status=="NS"]<-0


fps<-readxl::read_excel("fps.xlsx")
lastseen<-left_join(lastseen, fps)
lastseen<-left_join(lastseen, birdid)

stillalive<-lastseen%>%filter(FieldPeriodID==186& Status!="NS")
birdsalive<-unique(stillalive$BirdID)
lastseen<-lastseen%>%
  filter(BirdID %!in% birdsalive)

lastseen<-lastseen%>%
  group_by(BirdID)%>%
  filter(Status!="NS")%>%
  filter(FieldPeriodID==max(FieldPeriodID))


#end of period last seen - birthday 
lastseen$PeriodEnd<-as.Date(lastseen$PeriodEnd, "%Y-%m-%d")
lastseen$BirthDate<-as.Date(lastseen$BirthDate, "%Y-%m-%d")
lastseen$lifespan<-lastseen$PeriodEnd-lastseen$BirthDate

lifespan<-lastseen%>%select(BirdID,lifespan)
lifespan$lifespan<-as.numeric(lifespan$lifespan)-1


alllifespan<-merge(maxlifespan, lifespan, by=c("BirdID",'lifespan'), all=T)


alllifespan<-alllifespan%>%
  group_by(BirdID)%>%
  filter(lifespan==min(lifespan))


names(alllifespan)<-c("BirdID","newlifespan")


# for(i in unique(lastseen$BirdID)){
#   onebird<-filter(lastseen, lastseen$BirdID==i)
#   if(all(onebird$survival!=0)){
#     lastyear<-max(onebird$PeriodYearEnd)#find the last year vector 
#     PeriodYear<-max(onebird$PeriodYear)
#     age<-max(onebird$age)
#     birthyear<-unique(onebird$birthyear)
#     lastseen<-lastseen%>%
#       add_row(BirdID=i, PeriodYear=PeriodYear+1, Status="NS", age=age+1, survival=0, birthyear=birthyear)
#     #and then add a new row into the dataframe and assign the survival as 0 
#   }
# }


write.csv2(alllifespan, "lifespan.csv")
############################################################



# lifespan<-read.csv("maxlifespan.csv",stringsAsFactors=F,sep = ";")
physio<-read.csv("physio_model.csv",stringsAsFactors = F)
oxy<-read.csv("oxy_model.csv", stringsAsFactors = F)
haematocrit<-read.csv("haematocrit_model.csv",stringsAsFactors = F)
telomeres<-read.csv("telomere_model.csv", stringsAsFactors = F)
malaria<-read.csv("malaria_model.csv",stringsAsFactors = F)
provisioning<-read.csv("provisioning_model.csv",stringsAsFactors = F)
reprosuccess<-read.csv("reprosuccess_model.csv",stringsAsFactors = F)

physio<-physio[,-c(1)]
physio<-left_join(physio, alllifespan, by="BirdID")
physio<-physio %>% filter(!is.na(newlifespan))
physio<-physio[,-c(29,30,38,39)]

physio$BodyMass_z<-scale(physio$BodyMass)
physio$WingLength_z<-scale(physio$WingLength)
physio$RightTarsus_z<-scale(physio$RightTarsus)


write.csv(physio, "physio_model.csv")
#######################################################

unwanted<-c("FL","EGG","CH")

oxy<-oxy[,-c(1,2)]
oxy<-left_join(oxy, alllifespan, by = "BirdID")
oxy<-oxy %>% filter(!is.na(newlifespan))
oxy<-oxy%>%filter(Status %!in% unwanted)
oxy$sum_oxy_z<-scale(oxy$sum_oxy)
oxy$sum_roms_z<-scale(oxy$sum_roms)

write.csv(oxy, "oxy_model.csv")

#################################

haematocrit<-haematocrit[,-c(1)]
haematocrit<-filter(haematocrit, haematocrit$BuffyCoat<5)
haematocrit<-left_join(haematocrit, alllifespan, by = "BirdID")
haematocrit<-filter(haematocrit, haematocrit$Island=='CN')
haem<-filter(haematocrit, !is.na(haematocrit$newlifespan))
haem<-filter(haem, haem$Status %!in% unwanted)
haem$wbc_z<-scale(haem$wbc)
haem$Cells_z<-scale(haem$Cells)


write.csv(haem, "haematocrit_model.csv")
##############################################

telomeres<-telomeres[,-c(1,2)]
telomeres<-left_join(telomeres, alllifespan, by = "BirdID")
telomeres<-filter(telomeres, telomeres$Status %!in% unwanted) #no ch or egg or fl 
telomeres<-telomeres%>%filter(!is.na(newlifespan))
telomeres$RTL_z<-scale(telomeres$RTL)

write.csv(telomeres, "telomere_model.csv")
##################################

malaria<-malaria[,-c(1)]
malaria<-left_join(malaria, alllifespan, by = "BirdID")
malaria<-filter(malaria, malaria$Status %!in% unwanted) #no ch or egg or fl 
malaria<-filter(malaria, !is.na(malaria$newlifespan))

write.csv2(malaria, "malaria_model.csv")
#######################################

provisioning<-provisioning[,-c(1)]

provisioning<-left_join(provisioning, alllifespan, by='BirdID')
provisioning<-filter(provisioning, !is.na(provisioning$newlifespan))
provisioning$prate_z<-scale(provisioning$prate)
provisioning$occasionyear<-provisioning$year

write.csv2(provisioning, "provisioning_model.csv")

#########################################
reprosuccess<-reprosuccess[,-c(1)]

reprosuccess<-left_join(reprosuccess, alllifespan, by='BirdID')
reprosuccess<-filter(reprosuccess, reprosuccess$Status %!in% unwanted)
reprosuccess<-filter(reprosuccess, !is.na(reprosuccess$newlifespan))

write.csv(reprosuccess, "repro_model.csv")


