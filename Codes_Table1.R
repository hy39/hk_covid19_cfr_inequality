library(ggplot2)
library(plyr)
library(ggpubr)
library(data.table)
library(carData)
library(car)
library(pscl)
library(ggrepel)
library(patchwork)
library(reshape2)
library(tidyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)
library(corrplot)
library(Hmisc)
options(ggrepel.max.overlaps = Inf)

Covid2<-read.csv("data/Covid model_symptomatic.csv")

#Age.Group with p-value--------------------------------------------------------------
Covid2_Discharged_Age<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Discharged"))%>%group_by(Age.Group)%>%count(Situation)
Covid2_Deceased_Age<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Deceased"))%>%group_by(Age.Group)%>%count(Situation)
v1<-c(0,1,"Deceased","Deceased",0,0)
newmatrix1<-matrix(v1,nrow=2)
colnames(newmatrix1)<-c("Age.Group","Situation","n")
new=as.data.frame(newmatrix1)
newMatrix2 <- rbind(new,Covid2_Deceased_Age)
Covid2_Situation_Age<-cbind(Covid2_Discharged_Age, newMatrix2)
Covid2_Situation_Age[,6] <- as.numeric(unlist(Covid2_Situation_Age[,6]))                       
Covid2_Situation_Age%>%mutate(Total=n...3+n...6)

#Count number 
df<- data.frame(Covid2$Age.Group,Covid2$Situation, stringsAsFactors = TRUE)
pvalue <- wilcox.test(Covid2$Age.Group~Covid2$Situation,
                      data = df,
                      exact = FALSE)
pvalue

# Mean median Sd
describe(Covid2$Age)
sd(Covid2$Age)

Discharged_Age<-Covid2[Covid2$Situation=="Discharged",]
describe(Discharged_Age$Age)
sd(Discharged_Age$Age)

Deceased_Age<-Covid2[Covid2$Situation=="Deceased",]
describe(Deceased_Age$Age)
sd(Deceased_Age$Age)

# Gender--------------------------------------------------------------
#Gender group count
Covid2_Discharged_Gender<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Discharged"))%>%group_by(Gender)%>%count(Situation)
Covid2_Deceased_Gender<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Deceased"))%>%group_by(Gender)%>%count(Situation)
Covid2_Situation_Gender<-cbind(Covid2_Discharged_Gender, Covid2_Deceased_Gender)
Covid2_Situation_Gender%>%mutate(Total=n...3+n...6)
# P value
chisq.test(Covid2$Gender,Covid2$Situation, correct=FALSE)

# Income.region--------------------------------------------------------------
# Count value
Covid2_Discharged_Income<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Discharged"))%>%group_by(Income.Region)%>%count(Situation)
Covid2_Deceased_Income<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Deceased"))%>%group_by(Income.Region)%>%count(Situation)
Covid2_Situation_Income<-cbind(Covid2_Discharged_Income, Covid2_Deceased_Income)
Covid2_Situation_Income%>%mutate(Total=n...3+n...6)
# P value
chisq.test(Covid2$Income.Region,Covid2$Situation, correct=FALSE)

# Hospitalization.Time--------------------------------------------------------------
# mean median sd
describe(Covid2$Hospitalization.time)
sd(Covid2$Hospitalization.time)

Discharged_df<-Covid2[Covid2$Situation=="Discharged",]
describe(Discharged_df$Hospitalization.time)
sd(Discharged_df$Hospitalization.time)

Deceased_df<-Covid2[Covid2$Situation=="Deceased",]
describe(Deceased_df$Hospitalization.time)
sd(Deceased_df$Hospitalization.time)

# P value
df2<- data.frame(Covid2$Hospitalization.time,Covid2$Situation, stringsAsFactors = TRUE)
pvalue2<- wilcox.test(Covid2$Hospitalization.time~Covid2$Situation,
                      data = df2,
                      exact = FALSE)
pvalue2

# Delay reporting time--------------------------------------------------------------
# Count value
Covid2_Discharged_Delay<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Discharged"))%>%group_by(Grouped.Delay.Time.2)%>%count(Situation)
Covid2_Deceased_Delay<-Covid2%>%group_by(Situation)%>%filter(any(Situation=="Deceased"))%>%group_by(Grouped.Delay.Time.2)%>%count(Situation)
Covid2_Situation_Delay<-cbind(Covid2_Discharged_Delay, Covid2_Deceased_Delay)
Covid2_Situation_Delay%>%mutate(Total=n...3+n...6)

#P value
df3<- data.frame(Covid2$Delay.Time,Covid2$Situation, stringsAsFactors = TRUE)
pvalue3 <- wilcox.test(Covid2$Delay.Time~Covid2$Situation,
                       data = df3,
                       exact = FALSE)
pvalue3

# mean median sd
describe(Covid2$Delay.Time)
sd(Covid2$Delay.Time)

Discharged_Delay<-Covid2[Covid2$Situation=="Discharged",]
describe(Discharged_Delay$Delay.Time)
sd(Discharged_Delay$Delay.Time)

Deceased_Delay<-Covid2[Covid2$Situation=="Deceased",]
describe(Deceased_Delay$Delay.Time)
sd(Deceased_Delay$Delay.Time)

# cfr calculation--------------------------------------------------------------
#CFR of symptomatic patients:
# Total
Covid2_Dead<-Covid2%>%group_by(event)%>%filter(any(event=="1"))%>%count(event)
Covid2_Alive<-Covid2%>%group_by(event)%>%filter(any(event=="0"))%>%count(event)
Covid2_CFR<-cbind(Covid2_Dead, Covid2_Alive)
colnames(Covid2_CFR)<-c("event_0","n.x","event_1","n.y")
Covid2_CFR%>%mutate(CFR=n.x/(n.x+n.y)*100)

# Gender
Covid2_Dead_Gender<-Covid2%>%group_by(event)%>%filter(any(event=="1"))%>%group_by(Gender)%>%count(event)
Covid2_Alive_Gender<-Covid2%>%group_by(event)%>%filter(any(event=="0"))%>%group_by(Gender)%>%count(event)
Covid2__Gender_CFR<-merge(Covid2_Dead_Gender, Covid2_Alive_Gender, by = "Gender",all = TRUE)
Covid2__Gender_CFR%>%group_by(Gender)%>%mutate(CFR=n.x/(n.x+n.y)*100)

# Income
Covid2_Income_Dead<-Covid2%>%group_by(event)%>%filter(any(event=="1"))%>%group_by(Income.Region)%>%count(event)
Covid2_Income_Alive<-Covid2%>%group_by(event)%>%filter(any(event=="0"))%>%group_by(Income.Region)%>%count(event)
Covid2_Income_CFR<-merge(Covid2_Income_Dead, Covid2_Income_Alive, by = "Income.Region",all = TRUE)
Covid2_Income_CFR%>%group_by(Income.Region)%>%mutate(CFR=n.x/(n.x+n.y)*100)

