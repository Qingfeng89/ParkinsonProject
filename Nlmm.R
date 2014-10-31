rm(list=ls())
#change the working directory based on your machine
setwd("/Users/qingfenghu/Documents/Research/Parkinson")
library(MASS)
library(nlme)
library(lattice)
library(xtable)

############################
PPMI <- read.csv("PPMI_Data1003_14.csv")
PPMItest <- PPMI[order(PPMI$PATNO),]
PPMI[which(is.na(PPMI$PDMEDYN)),]$PDMEDYN <- -1
on_index <- which(PPMI$PDMEDYN == 1)
temp_index <- PPMI[on_index,]$PATNO
PPMI_onmed <- PPMI[which(PPMI$PATNO %in% temp_index),]
PPMI_ontest <- PPMI_onmed[order(PPMI_onmed$PATNO),]
unitemp <- unique(temp_index)
for (id in unitemp){
  pos <- which(PPMI_onmed[which(PPMI_onmed$PATNO==id),]$PDMEDYN == 1)
  if(!(any(pos %in% c(1,2,3,4,5,6))))
    temp_index <- temp_index[-which(temp_index == id)]
}
PPMI_offmed <- PPMI[which(!(PPMI$PATNO %in% temp_index)),]
#PPMI_offtest <- PPMI_offmed[order(PPMI_offmed$PATNO),]
#PPMI_offmed <- PPMI[off_index,]
name <- c("MDS_UPDRS_I", "MDS_UPDRS_II", "MDS_UPDRS_III",
          "MDS_UPDRS", "BJLOS", "ESS", "GDSRS", "HVLTIR",
          "HVLTDG","HVLTRDLY", "LNS", "MOCA", "QUIP", "REMSBD", "SCOPA_AUT",
          "SFT", "STAT", "STASS", "STATS", "UPSITRS", "MCI",
          "CONTRALATERAL", "IPSILATERAL", "CAUDATE", "PUTAMEN",
          "STRIATUM", "CDR", "AI", "ABETA", "TAU", "PTAU",
          "ALPHA", "TAU_ABETA", "PTAU_ABETA", "PTAU_TAU",
          "URATE")
#PPMI_PD <- PPMI_offmed[which(PPMI_offmed$DIAG==1),]
#PPMI_HC <- PPMI_offmed[which(PPMI_offmed$DIAG==2),]
#numPD <- length(unique(PPMI_PD$PATNO))
#num_HC <- length(unique(PPMI_HC$PATNO))
#levels(PPMI_PD$TIME) <- c(0,60,-1,3,6,9,12,18,24,30,36,42,48,54)
#PPMI_PD$TIME <- as.numeric(as.character(PPMI_PD$TIME))
PPMI_offmed <- PPMI_offmed[which(PPMI_offmed$DIAG!=3),]
PPMI_offmed$DIAG <- PPMI_offmed$DIAG *(-1) + 2 #HC = 0 PD = 1
PPMI_offmed$DIAG <- factor(PPMI_offmed$DIAG)
levels(PPMI_offmed$TIME) <- c(0,60,-1,3,6,9,12,18,24,30,36,42,48,54)
PPMI_offmed$TIME <- as.numeric(as.character(PPMI_offmed$TIME))
stoplist <- NULL
################
for (i in 1:26){
  PPMI_MDS1 <- as.data.frame(PPMI_offmed[,c("PATNO","TIME","DIAG",name[i])])
  names(PPMI_MDS1) <- c("PATNO","TIME","DIAG",name[i])
  #PPMI_MDS1 <- PPMI_MDS1[order(PPMI_MDS1$PATNO),]
  #write.csv(x=PPMI_MDS1,file="/Users/qingfenghu/Documents/Research/Parkinson/PPMI_MDS.csv",row.names=F)
  PPMI_MDS1.PDmodel<- try(lme(as.formula(paste(name[i],'~ TIME*DIAG')), random = ~ 1 + TIME | PATNO, data= PPMI_MDS1,na.action = na.exclude),TRUE)
  if(isTRUE(class(PPMI_MDS1.PDmodel)=="try-error")){
    stoplist <- c(stoplist,i)
  }
}
PPMI_MDS1 <- as.data.frame(PPMI_offmed[,c("PATNO","TIME","DIAG",name[i])])
names(PPMI_MDS1) <- c("PATNO","TIME","DIAG",name[i])
#PPMI_MDS1 <- PPMI_MDS1[order(PPMI_MDS1$PATNO),]
#write.csv(x=PPMI_MDS1,file="/Users/qingfenghu/Documents/Research/Parkinson/PPMI_MDS.csv",row.names=F)
PPMI_MDS1.PDmodel<- lme(as.formula(paste(name[i],'~ TIME*DIAG')), random = ~ 1 + TIME | PATNO, data= PPMI_MDS1,na.action = na.exclude)

#summary(PPMI_MDS1.PDmodel)
pdata <- expand.grid(TIME=c(-1,0,3,6,9,12,18,24,30,36,42,48,54,60), DIAG=c(0,1))
pdata$Marker <- predict(PPMI_MDS1.PDmodel, pdata, level=0)
file = paste("mylme",1,".pdf",sep="")
pdf(file=file,paper='special',width=7,height=9)
plot(pdata$TIME, pdata$Marker, type="n",xlab="TIME (Months)", ylab=name[i])
points(pdata$TIME[1:14], pdata$Marker[1:14], type="b", pch=19, lwd=2)
points(pdata$TIME[15:28], pdata$Marker[15:28], type="b", pch=22, lty=2, lwd=2)
legend("topright",c("HC","PD"),pch=c(19, 22), lty=c(1,2), lwd=2)
dev.off()

