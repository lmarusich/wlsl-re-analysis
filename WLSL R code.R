#This package allows you to load in files from SPSS
library(foreign)

#This package allows you to run multi-level models
library(lme4)

#Allows statistical tests from MLM
library(lmerTest)

#robust estimates in mlm
library(robustlmm)

#no idea
library(arm)

library(sjPlot)
library(BootMRMR)
library(ggplot2)
library(dplyr)

#changes the working directory to whereever
# setwd('C:/Users/Michael Geuss/Documents/ARL')

#read in csv files with variable names as header
# RTeff <- read.csv(file="RTLongCoreff.csv",header=TRUE,sep=",")

#read in SPSS file
# data = read.spss("s5Longfile_OLremoved.sav", use.value.labels = TRUE, to.data.frame = TRUE)
data <- read.csv('s5Longfile_OLremoved.csv', stringsAsFactors = F)
summary(data)


#subset into dist est only

Ddata <- data[ which(data$estimate_type=='Distance'), ]

#split file between Tog ony and SW
Ddata$condition <- as.numeric(Ddata$condition)

colnames(Ddata)[1] <- "subject"

#SL = 2
#WL = 1
#Toggle = 0

DdataTog <- Ddata[which(Ddata$condition == 0),]
DdataSW <- Ddata[which(Ddata$condition > 0),]



#check for normality
DdataSW$predict <- predict(Y)
DdataSW$resid <- DdataSW$subject_estimate - DdataSW$predict
DdataSW$zresid <- (DdataSW$resid - mean(DdataSW$resid))/sd(DdataSW$resid)
hist(DdataSW$zresid)

#heteroscedasticity
plot(DdataSW$zresid ~ DdataSW$stim_distance)
#change stim_distance to other variables

#random slope model
rslope <- lmer(subject_estimate ~ stim_distance + (1 + stim_distance|subject), data = DdataSW)


# Mlm
Y <- lmer(subject_estimate ~ stim_distance*stim_heading*stim_number*condition + (1|subject),
          data = DdataSW, REML = 0)
summary (Y)

X <- lmer(Ratio ~ 1 + stim_distance*stim_heading*stim_number*condition + (1|subject),
          data = DdataSW, REML = 0)
summary (X)

Z <- lmer(AbsoluteError ~ 1 + stim_distance*stim_heading*stim_number*condition + (1|subject),
          data = DdataSW, REML = 0)
summary (Z)

#basic interaction plot
plot_model(Y, type = "int", mdrt.values = 'meansd' , show.data = F)
plot_model(X, type = "int", mdrt.values = 'meansd' , show.data = F)
plot_model(Z, type = "int", mdrt.values = 'meansd' , show.data = F)
