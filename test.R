rm(list=ls())

library(lars)

BodyFat=read.csv("BodyFat.csv")
dim(BodyFat)
colnames(BodyFat)
head(BodyFat) 
#IDNO is the number of observations
#BODYFAT is the dependent variable.
#The unit of weight is lb and the unit of height is inches.

#Data cleaning
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
#Based on the rule of thumb, we remove the observations with 3 times of mean. They are 42 and 163.
new.BodyFat<-BodyFat[-which((BMI-BodyFat$ADIPOSITY)>3*mean(diff)),]
new.BMI<-BodyFat$WEIGHT*703/((BodyFat$HEIGHT)^2)
new.ADIPOSITY<-BodyFat$ADIPOSITY
new.diff<-BMI-ADIPOSITY
mean(new.diff)

#The future data cleaning will be removing the outliers or extreme points in simple linear model.

#1. Multiple linear regression with all the independent variables
lm1<-lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data=BodyFat)
summary(lm1)

#2. Using stepwise regression
lm2.1<-step(lm1, direction = "both", k=2) # both.sided + AIC 
summary(lm2.1)
lm2.2<-step(lm1, direction = "forward", k=2) # forward + AIC 
summary(lm2.2)
lm2.3<-step(lm1, direction = "backward", k=2) # backward + AIC 
summary(lm2.3)

lm3.1<-step(lm1, direction = "both", k=log(length(BodyFat$BODYFAT))) # both.sided + BIC 
summary(lm3.1)

lm3.2<-step(lm1, direction = "forward", k=log(length(BodyFat$BODYFAT))) # forward + AIC 
summary(lm3.2)

lm3.3<-step(lm1, direction = "backward", k=log(length(BodyFat$BODYFAT))) # backward + AIC 
summary(lm3.3)

#3. Only selecting the obvious variables in the lm2 
lm4<-lm(BODYFAT~AGE+WEIGHT+NECK+ABDOMEN+THIGH+FOREARM+WRIST,data=BodyFat)
summary(lm4)
plot(lm4)
#4. LASSO Regression
x<-as.matrix(BodyFat[,4:17])
y<-as.matrix(BodyFat[,2])
lm5<-lars(x,y,type = "lasso")
lm5
plot(lm5)
summary(lm5)#Select step=11, Cp=9.5408
lm5$beta
coef <-coef.lars(lm5,mode="step",s=12)
coef

?step
