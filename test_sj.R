rm(list=ls())

BodyFat=read.csv("BodyFat.csv",row.names = 1)
dim(BodyFat)
colnames(BodyFat)
head(BodyFat)

# 1. Find outliers for each predictor by boxplot
Outliers=apply(BodyFat,2,function(x){which(x%in%boxplot.stats(x)$out)})
Outliers
sort(table(unlist(Outliers)),decreasing = TRUE)
### 39 is listed to be an outlier in 11 coloumns
### 41 is regarded as an outlier in 7 coloumns
### 216 is regarded as an outlier in 4 coloumns
### Totally 19 outliers 

# 2. Flag invalid points (probably caused by documented error)
## 2.1 Density vs Bodyfat
pairs(BodyFat[,c(1,2)])
BODYFAT.x=495/BodyFat$DENSITY-450
plot(BODYFAT.x-BodyFat$BODYFAT,pch=20)
flag1=which((BODYFAT.x-BodyFat$BODYFAT)%in%boxplot.stats(BODYFAT.x-BodyFat$BODYFAT)$out)
### 48,76,96,182,216
### Alternatively, by linear regression
# lm.flag1=lm(DENSITY~BODYFAT,data=BodyFat)
# flag1=sort(abs(lm.flag1$residuals),decreading=TRUE)[1:5]
# BodyFat[names(flag1),]

## 2.2 Adioposity(bmi) vs Height&Weight
bmi=BodyFat$WEIGHT*703/(BodyFat$HEIGHT)^2
plot(bmi-BodyFat$ADIPOSITY)
flag2=which((bmi-BodyFat$ADIPOSITY)%in%boxplot.stats(bmi-BodyFat$ADIPOSITY)$out)
### 42, 163, 221 
### 42, 46, 61, 86, 116, 136, 156, 163, 221, 235

## 2.3 Bodyfat less than 2
flag3=which(BodyFat$BODYFAT<=2)
### 172,182

## 2.4 Remove incorrect data points
rm.index=unique(c(flag1,flag2,flag3))
new.BodyFat=BodyFat[-rm.index,]
write.csv(new.BodyFat,file="BodyFat_C.csv")
####Outliers are not removed

# 3 Regression
lm=lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data=BodyFat)
summary(lm)
## MSE
mean((lm$residuals)^2) #15.01733

dat=read.csv("BodyFat_C.csv",header = TRUE,row.names = 1)
lm1=lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data=dat)
summary(lm1)
## MSE
mean((lm1$residuals)^2) #14.48052

lm2=step(lm1,direction = "backward",k=2)
summary(lm2)
## MSE
mean((lm2$residuals)^2) #14.62169

lm3=step(lm1,direction = "backward",k=log(dim(dat)[1]))
summary(lm3)
## MSE
mean((lm3$residuals)^2) #15.208
