rm(list=ls())
#change the working directory based on your machine
setwd("/Users/qingfenghu/Documents/Research/Parkinson0215/")
library(MASS)
library(nlme)
library(lattice)
library(xtable)
####
PPMI_offmed <- read.csv("Offmedfull.csv")
PPMI_offmed <- PPMI_offmed[which(PPMI_offmed$DIAG!=3),]
PPMI_offmed$DIAG <- PPMI_offmed$DIAG *(-1) + 2 #HC = 0 PD = 1
PPMI_offmed$DIAG <- factor(PPMI_offmed$DIAG)
levels(PPMI_offmed$TIME) <- c(0,60,-1,3,6,9,12,18,24,30,36,42,48,54)
PPMI_offmed$TIME <- as.numeric(as.character(PPMI_offmed$TIME))
#offmed_PD <- PPMI_offmed[which(PPMI_offmed$DIAG ==1),]
stoplist <- NULL
name <- c("MDS_UPDRS_I", "MDS_UPDRS_II", "MDS_UPDRS_III",
          "MDS_UPDRS", "BJLOS", "ESS", "GDSRS", "HVLTIR",
          "HVLTDG","HVLTRDLY", "LNS", "MOCA", "QUIP", "REMSBD", "SCOPA_AUT",
          "SFT", "STAT", "STASS", "STATS", "UPSITRS", "MCI",
          "CONTRALATERAL", "IPSILATERAL", "CAUDATE", "PUTAMEN",
          "STRIATUM", "CDR", "AI", "ABETA", "TAU", "PTAU",
          "ALPHA", "TAU_ABETA", "PTAU_ABETA", "PTAU_TAU",
          "URATE")
i = 3
PPMI_MDS1 <- as.data.frame(PPMI_offmed[,c("PATNO","TIME","DIAG",name[i])])
names(PPMI_MDS1) <- c("PATNO","TIME","DIAG",name[i])
PPMI_MDS1.PDmodel<- lme(as.formula(paste(name[i],'~ TIME*DIAG')), random = ~ 1 + TIME | PATNO, data= PPMI_MDS1,na.action = na.exclude)


#
#for (i in 1:36){
#  PPMI_MDS1 <- as.data.frame(offmed_PD[,c("PATNO","TIME","DIAG",name[i])])
#  names(PPMI_MDS1) <- c("PATNO","TIME","DIAG",name[i])
#  PPMI_MDS1.PDmodel<- lme(as.formula(paste(name[i],'~ TIME+DIAG')), random = ~ 1 + TIME | PATNO, data= PPMI_MDS1,na.action = na.exclude)
#  PPMI_MDS1.PDmodel<- try(lme(as.formula(paste(name[i],'~ TIME+DIAG')), random = ~ 1 + TIME | PATNO, data= PPMI_MDS1,na.action = na.exclude),TRUE)
#  if(isTRUE(class(PPMI_MDS1.PDmodel)=="try-error")){
#    stoplist <- c(stoplist,i)
#  }
#}

pdata <- expand.grid(TIME=c(-1,0,3,6,9,12,18,24,30,36,42,48,54,60), DIAG=factor(c(0,1)))
# predict response
pdata$Marker <- predict(PPMI_MDS1.PDmodel, pdata, level=0)
# create design matrix
designmat <- model.matrix(eval(eval(PPMI_MDS1.PDmodel$call$fixed)[-2]), pdata[-ncol(pdata)])
#compute standard error for predictions
predvar <- diag(designmat %*% PPMI_MDS1.PDmodel$varFix %*% t(designmat))
pdata$SE <- sqrt(predvar) 
pdata$SE2 <- sqrt(predvar+PPMI_MDS1.PDmodel$sigma^2)
# plot standard deviation of fitting
library(ggplot2) 
p1 <- ggplot(pdata,aes(x=TIME,y=Marker,group=DIAG,color=DIAG)) +
  geom_line() +
  geom_ribbon(aes(ymin=Marker-2*SE2,ymax=Marker+2*SE2),alpha=0.2,fill="blue")+ 
  geom_ribbon(aes(ymin=Marker-2*SE,ymax=Marker+2*SE),alpha=0.2,fill="green")+ 
  geom_point(data=PPMI_MDS1,aes(x=TIME,y=MDS_UPDRS_III,group=DIAG))+
  scale_colour_discrete(name="Group Of Subjects",breaks=c("0", "1"),labels=c("Healthy Subject", "PD Patient"))+
  labs(x="TIME",y="MDS_UPDRS_III",title="Standard Error Of Fitting")
p1

# plot Residual VS fitting
qqnorm(residuals(PPMI_MDS1.PDmodel))
qqline(residuals(PPMI_MDS1.PDmodel))
plot(fitted(PPMI_MDS1.PDmodel), residuals(PPMI_MDS1.PDmodel),xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2,col="red",lwd=2)
#lines(smooth.spline(fitted(PPMI_MDS1.PDmodel), residuals(PPMI_MDS1.PDmodel)))

