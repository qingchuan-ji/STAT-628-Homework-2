rm(list=ls())

library(lars)

BodyFat=read.csv("BodyFat.csv")
dim(BodyFat)
colnames(BodyFat)
head(BodyFat) 
#IDNO is the number of observations
#BODYFAT is the dependent variable.
#The unit of weight is lb and the unit of height is inches.

#1. Multiple linear regression with all the independent variables
lm1<-lm(BODYFAT~DENSITY+AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data=BodyFat)
summary(lm1)

#2. Using stepwise regression
lm2<-step(lm1)
summary(lm2)

#3. Only selecting the obvious variables in the lm2 
lm3<-lm(BODYFAT~DENSITY+AGE+WEIGHT,data=BodyFat)
summary(lm3)
plot(lm3)
#4. LASSO Regression
x<-as.matrix(BodyFat[,3:17])
y<-as.matrix(BodyFat[,2])
lm4<-lars(x,y,type = "lasso")
lm4
plot(lm4)
summary(lm4)#Select step=4, Cp=0.2283
lm4$beta
coef <-coef.lars(lm4,mode="step",s=5)
coef
