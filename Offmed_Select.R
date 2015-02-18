rm(list=ls())
#change the working directory based on your machine
setwd("/Users/qingfenghu/Documents/Research/Parkinson0215/")
library(MASS)
# PPMI_offmed just set the subtype from BL to month before medication same as the subtype of last month bf med,
#PPMI_offmed2 set the subtype from BL to month before medication same as the 
#last month before start medication. Once subjects start medication,the subtype would be set as NA,
#and measurement would be set as NA. 
PPMI <- read.csv("PPMI_150218.csv")
PPMI[which(is.na(PPMI$PDMEDYN)),]$PDMEDYN <- -1
PPMI_offmed <- PPMI
PPMI_offmed$flagpd <- PPMI_offmed$PDMEDYN # -1 is NA
on_index <- which(PPMI$PDMEDYN == 1)
notsure_index <- which(PPMI$PDMEDYN == -1 & PPMI$DIAG == 1)
all_index <- c(on_index,notsure_index)
temp_index <- PPMI[all_index,]$PATNO
PPMI_onmed <- PPMI[which(PPMI$PATNO %in% temp_index),]
unitemp <- unique(temp_index)
TIME1 <- c("SC", "BL", "V01", "V02", "V03", "V04", "V05", "V06", 
          "V07","V08", "V09", "V10", "V11", "PW") #PW=V12
PPMI_offmed2 <- PPMI_offmed
IdList <- NULL
for (id in unitemp){
  pos <- which(PPMI_onmed[which(PPMI_onmed$PATNO==id),]$PDMEDYN == 1)
  if(length(pos)==0){
    pos <- which(PPMI_onmed[which(PPMI_onmed$PATNO==id),]$PDMEDYN == 0) # has 0
    if(length(pos)==0){
      PPMI_offmed <- PPMI_offmed[-which(PPMI_offmed$PATNO == id),]
      PPMI_offmed2 <- PPMI_offmed2[-which(PPMI_offmed2$PATNO == id),]
      IdList <- c(IdList,id) # Id of subjects whose PD medication is NA from beginning; they are removed from the list
      next
    } #all NA
    pos <- pos + length(pos)
    if(pos[1]>length(TIME1))
      pos[1] = length(TIME1)
  }
  time <- TIME1[pos[1]:length(TIME1)]
  timeoff <- TIME1[1:pos[1]-1]
  last_bf_md <- tail(timeoff,n=1)
  PPMI_offmed <- PPMI_offmed[-which(PPMI_offmed$PATNO == id & PPMI_offmed$TIME %in% time),]
  subtype <- PPMI_offmed[which(PPMI_offmed$PATNO == id & PPMI_offmed$TIME == last_bf_md ),]$TD_PIGD
  PPMI_offmed[which(PPMI_offmed$PATNO == id & PPMI_offmed$TIME %in% timeoff),]$TD_PIGD <- subtype
  PPMI_offmed2[which(PPMI_offmed2$PATNO == id & PPMI_offmed2$TIME %in% time),]$TD_PIGD <- NA # NA for LMM subtype
  PPMI_offmed2[which(PPMI_offmed2$PATNO == id & PPMI_offmed2$TIME %in% timeoff),]$TD_PIGD <- subtype
  PPMI_offmed2[which(PPMI_offmed2$PATNO == id & PPMI_offmed2$TIME %in% time),]$flagpd <- 0 # represents for on medication
  PPMI_offmed2[which(PPMI_offmed2$PATNO == id & PPMI_offmed2$TIME %in% timeoff),]$flagpd <- 1 # represents for off medication
  PPMI_offmed2[which(PPMI_offmed2$PATNO == id & PPMI_offmed2$TIME %in% time),11:46] <- NA
}
write.csv(x=PPMI_offmed, file="/Users/qingfenghu/Documents/Research/Parkinson0215/Offmed_v1.csv", row.names=F)
#PPMI_offtest <- PPMI_offmed[order(PPMI_offmed$PATNO),]
#write.csv(x=PPMI_offtest, file="/Users/qingfenghu/Documents/Research/Parkinson1103/Off_med_sort_1119_v1.csv", row.names=F)
write.csv(x=PPMI_offmed2,file="/Users/qingfenghu/Documents/Research/Parkinson0215/Offmedfull.csv",row.names=F)
