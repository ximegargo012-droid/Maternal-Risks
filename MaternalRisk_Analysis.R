#Libraries to manage data
library('tidyr')
library('dplyr')

#Libraries to plot correlation
library(corrplot)
library(tidyverse)


#download cvs file: https://www.kaggle.com/datasets/joebeachcapital/maternal-health-risk
MaternalRisk <- read.csv('Maternal_Health_Risk_Data_Set.csv')
head(MaternalRisk)

#index of order by max prev and current scores
i <- order(-MaternalRisk$BS,-MaternalRisk$SystolicBP)
MaternalRisk2 <- MaternalRisk[i,]
head(MaternalRisk2)


#Filtering just numeric values, high risk.
MaternalRiskNum <- MaternalRisk %>% select(Age, SystolicBP, DiastolicBP, BS, BodyTemp, HeartRate)
head(MaternalRiskNum)

n <- length(MaternalRisk$RiskLevel)

for(i in 1:n){
  if(MaternalRisk$RiskLevel[i]=="high risk"){
    MaternalRiskNum$RiskLevel[i]=100
  }
  else if(MaternalRisk$RiskLevel[i]=="mid risk"){
    MaternalRiskNum$RiskLevel[i]=50
    }
  else{
    MaternalRiskNum$RiskLevel[i]=0
  }
}

head(MaternalRiskNum)

#Calculating correlation & plotting
plot(MaternalRiskNum)
cor(MaternalRiskNum)


C <- round(cor(MaternalRiskNum),1)

corrplot(C,method="pie",type="upper")

#Highest correlation
ggplot(data = MaternalRisk ) + 
  geom_point(aes(x=Age,y=BS, col=RiskLevel))

#Predictive model
modelHR <- lm(RiskLevel ~ BS+SystolicBP , MaternalRiskNum)

#Cheking data
modelHR
summary(modelHR)
View(modelHR)


#Adding residuals and fitted values for predictive model
MaternalRiskNum$residuals <- modelHR$residuals
MaternalRiskNum$predicted <- modelHR$fitted.values
MaternalRiskNum


#predictions for not in dataset
Wish <- data.frame(BS=c(6.10,12),SystolicBP=c(120,130))
PV <- predict(modelHR,Wish)
PV

n2 <- length(PV)

for (i in 1:n2){
  if(PV[i]>=100){
    PV[i]="High risk"
  }
  else if(PV[i]<100 && PV[i] >=50){
    PV[i]="mid risk"
  }
  else{
    PV[i]="low risk"
  }
}

PV

