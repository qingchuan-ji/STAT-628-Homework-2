#Data cleaning part
rm(list=ls())

library(lars)

BodyFat=read.csv("BodyFat.csv")
dim(BodyFat)
colnames(BodyFat)
head(BodyFat) 

#1. Find Bodyfat less than 2 or 4

BodyFat$IDNO[which(BodyFat$BODYFAT<=2)]
BodyFat$IDNO[which(BodyFat$BODYFAT<=4)]

#Thus, we remove observation 172 and 182
BodyFat<-BodyFat[-which(BodyFat$BODYFAT<=4),]

#2. Find the difference in BMI and ADIPOSITY (data documented error)
BMI<-BodyFat$WEIGHT*703/((BodyFat$HEIGHT)^2)
ADIPOSITY<-BodyFat$ADIPOSITY
diff<-BMI-ADIPOSITY
mean(diff)
plot(diff)
BodyFat$IDNO[which((BMI-BodyFat$ADIPOSITY)>3*mean(diff))]
#Based on the rule of thumb, we remove the observations with 3 times of mean
new.BodyFat<-BodyFat[-which((BMI-BodyFat$ADIPOSITY)>3*mean(diff)),]
new.BMI<-BodyFat$WEIGHT*703/((BodyFat$HEIGHT)^2)
new.ADIPOSITY<-BodyFat$ADIPOSITY
new.diff<-BMI-ADIPOSITY
mean(new.diff)

#The future data cleaning will be removing the outliers or extreme points in simple linear model.
