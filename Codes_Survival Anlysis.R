library(survival)
library(ggplot2)
library(plyr)
library(ggpubr)
library(data.table)
library(car)
library(survminer)
library(dplyr)
library(lubridate)

#Survival Analysis
Covid<-read.csv("data/Covid model.csv")
Covid2<-read.csv("data/Covid model_symptomatic.csv")

#Subset Data for survival
Survival<-subset(Covid,select = -c(Number,Situation,district,Income,Population.Density,Income.Region2,Report,Onset,Outdate,Delay.Time,Numeric_Gender))
Survival2<-subset(Covid2,select = -c(Report,Onset,Outdate,Grouped.Delay.Time))

Survival$Gender<-factor(Survival$Gender,levels=c("F","M"))
Survival$Age.Group<-as.factor(Survival$Age.Group)
Survival$Age.Group<-factor(Survival$Age.Group,levels=c("0","1","2","3","4"))
Survival$Age.Group2<-factor(Survival$Age.Group2,levels=c("45-64","0-44","65+"))

Survival2$Gender<-factor(Survival2$Gender,levels=c("F","M"))
Survival2$Age.Group<-factor(Survival2$Age.Group,levels=c("0","1","2","3","4"))
Survival2$Age.Group2<-factor(Survival2$Age.Group2,levels=c("45-64","0-44","65+"), labels=c("45-64","0-44","65+"))
Survival2$Grouped.Delay.Time.2<-factor(Survival2$Grouped.Delay.Time.2,levels=c("0-10",">10"))

# Create binary variable for Age and Income in symptomatic patients
Survival2$Age.Group3<-Survival2$Age.Group2
Survival2$Age.Group3 <- as.character(Survival2$Age.Group3)
Survival2$Age.Group3[which(Survival2$Age.Group3== '45-64')] <- '0-64'
Survival2$Age.Group3[which(Survival2$Age.Group3== '0-44')] <- '0-64'
Survival2$Income.Region3<-Survival2$Income.Region
Survival2$Income.Region3[Survival2$Income.Region3== 'High Income Region'] <- 'High and Middle Income Region'
Survival2$Income.Region3[Survival2$Income.Region3== 'Middle Income Region'] <- 'High and Middle Income Region'

# Create binary variable for Age and Income in total patients
Survival$Age.Group3<-Survival$Age.Group2
Survival$Age.Group3 <- as.character(Survival$Age.Group3)
Survival$Age.Group3[which(Survival$Age.Group3== '45-64')] <- '0-64'
Survival$Age.Group3[which(Survival$Age.Group3== '0-44')] <- '0-64'
Survival$Income.Region3<-Survival$Income.Region
Survival$Income.Region3[Survival$Income.Region3== 'High Income Region'] <- 'High and Middle Income Region'
Survival$Income.Region3[Survival$Income.Region3== 'Middle Income Region'] <- 'High and Middle Income Region'

##Kaplan-Meier (K-M) non-parametric analysis of all Covid-19 patients##
KMSurvival<-survfit(Surv(Hospitalization.time,event) ~ 1, data= Survival)
summary(KMSurvival)
KMPlot<-ggsurvplot(KMSurvival,
                   conf.int = T, risk.table = F,
                   palette = "lancet", xlab= "", ylab= "Survival probability",
                   surv.scale="percent",
                   censor= T, censor.shape= 124, censor.size= 2,
                   font.legend = list(size = 24, face = "bold"),
                   axes.offset=FALSE, break.x.by=10,break.y.by=0.1,
                   ylim= c(0.5,1), 
                   ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                   legend.title="") + 
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot<-KMPlot$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot

##Kaplan-Meier (K-M) non-parametric analysis by group##
#Gender group
KMSurvival_Gender <- survfit(Surv(Hospitalization.time,event) ~ Gender, data=Survival)
summary(KMSurvival_Gender)
KMPlot_Gender<-ggsurvplot(KMSurvival_Gender,
                          xlab= "", ylab= "Survival probability",
                          legend.title="Gender", pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                          conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                          legend.labs= c("Female","Male"),
                          ylim= c(0.5,1),
                          risk.table.fontsize = 5.5,
                          censor= T, censor.shape= 124, censor.size= 2,
                          risk.table = F, palette = "lancet",
                          surv.scale="percent", 
                          ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                          axes.offset=FALSE, break.x.by=10,break.y.by=0.1) + 
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot_Gender<-KMPlot_Gender$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot_Gender

#Age group
KMSurvival_Age <- survfit(Surv(Hospitalization.time,event) ~ Age.Group, data=Survival)
summary(KMSurvival_Age)
KMPlot_Age<-ggsurvplot(KMSurvival_Age,
                       xlab= "Day", ylab= "Survival probability",
                       legend.title="Age",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                       conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                       legend.labs= c("0-14","15-24","25-44","45-64","65+"),
                       ylim= c(0.5,1),
                       risk.table.fontsize = 5.5,
                       censor= T, censor.shape= 124, censor.size= 2,
                       risk.table = F, palette = "lancet",
                       surv.scale="percent",
                       ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                       axes.offset=FALSE, break.x.by=10,break.y.by=0.1) +
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot_Age<-KMPlot_Age$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot_Age

#Income Regions Group
KMSurvival_Income <- survfit(Surv(Hospitalization.time,event) ~ Income.Region3, data=Survival)
summary(KMSurvival_Income)
KMPlot_Income<-ggsurvplot(KMSurvival_Income,
                          xlab= "Time since reporting(day)", ylab= "Survival probability",
                          legend.title="Income region",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                          conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                          legend.labs= c("High/middle\nincome region","Low income\n region"),
                          ylim= c(0.5,1),
                          risk.table.fontsize = 5.5,
                          censor= T, censor.shape= 124, censor.size= 2,
                          risk.table = F, palette = "lancet",
                          surv.scale="percent", 
                          ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                          axes.offset=FALSE, break.x.by=10,break.y.by=0.1) + 
  guides(color=guide_legend(nrow=2,override.aes = list(size = 2)))

KMPlot_Income<-KMPlot_Income$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot_Income

FigureS9<-ggarrange(KMPlot,KMPlot_Gender,KMPlot_Age,KMPlot_Income,
                    labels = c("A","B","C","D"), font.label = list(size = 12),
                    ncol = 2, nrow = 2)
FigureS9

#Kaplan-Meier (K-M) non-parametric analysis of Symptomatic Patients
KMSurvival2<-survfit(Surv(Hospitalization.time,event) ~ 1, data= Survival2)
summary(KMSurvival2)
KMPlot2<-ggsurvplot(KMSurvival2,
                    conf.int = T, risk.table = F,
                    palette = "lancet",xlab= "",
                    surv.scale="percent",
                    censor= T, censor.shape= 124, censor.size= 2,
                    font.legend = list(size = 24, face = "bold"),
                    axes.offset=FALSE, break.x.by=10,break.y.by=0.1,
                    ylim= c(0.5,1), 
                    ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                    legend.title="") + 
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot2<-KMPlot2$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot2

##Kaplan-Meier (K-M) non-parametric analysis by group##
#Gender group
KMSurvival2_Gender <- survfit(Surv(Hospitalization.time,event) ~ Gender, data=Survival2)
summary(KMSurvival2_Gender)
KMPlot2_Gender<-ggsurvplot(KMSurvival2_Gender,
                           xlab= "", ylab= "Survival probability",
                           legend.title="Gender",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                           conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                           legend.labs= c("Female","Male"),
                           ylim= c(0.5,1),
                           risk.table.fontsize = 5.5,
                           censor= T, censor.shape= 124, censor.size= 2,
                           risk.table = F, palette = "lancet",
                           surv.scale="percent", 
                           ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                           axes.offset=FALSE, break.x.by=10,break.y.by=0.1) + 
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot2_Gender<-KMPlot2_Gender$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot2_Gender

#Age group
KMSurvival2_Age <- survfit(Surv(Hospitalization.time,event) ~ Age.Group, data=Survival2)
summary(KMSurvival2_Age)
KMPlot2_Age<-ggsurvplot(KMSurvival2_Age,
                        xlab= "", ylab= "Survival probability",
                        legend.title="Age",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                        conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                        legend.labs= c("0-14","15-24","25-44","45-64","65+"),
                        ylim= c(0.5,1),
                        risk.table.fontsize = 5.5,
                        censor= T, censor.shape= 124, censor.size= 2,
                        risk.table = F, palette = "lancet",
                        surv.scale="percent", 
                        ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                        axes.offset=FALSE, break.x.by=10,break.y.by=0.1) + 
  guides(color=guide_legend(override.aes = list(size = 2)))

KMPlot2_Age<-KMPlot2_Age$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot2_Age

#Income Regions Group
KMSurvival2_Income <- survfit(Surv(Hospitalization.time,event) ~ Income.Region3, data=Survival2)
summary(KMSurvival2_Income)
KMPlot2_Income<-ggsurvplot(KMSurvival2_Income,
                           xlab= "Time since reporting(day)", ylab= "Survival probability",
                           legend.title="Income region",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                           legend.labs= c("High & middle\nincome region","Low income\nregion"),
                           conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 24, face = "bold"),
                           ylim= c(0.5,1),
                           risk.table.fontsize = 5.5,
                           censor= T, censor.shape= 124, censor.size= 2,
                           risk.table = F, palette = "lancet",
                           surv.scale="percent", 
                           ggtheme = theme_survminer(font.caption = c(24),font.x = c(24),font.y = c(24),font.tickslab = c(24)),
                           axes.offset=FALSE, break.x.by=10,break.y.by=0.1) + 
  guides(color=guide_legend(nrow=2,override.aes = list(size = 2)))

KMPlot2_Income<-KMPlot2_Income$plot+ coord_cartesian(xlim = c(0, 105))
KMPlot2_Income

FigureS7<-ggarrange(KMPlot2,KMPlot2_Gender,KMPlot2_Age,KMPlot2_Income,
                   labels = c("A","B","C","D"), font.label = list(size = 19),
                   ncol = 2, nrow = 2)
FigureS7

#Cox proportional hazard model & Assumption#
# Table S7, estimates of Cox regression for total patients
Coxph_Total<- coxph(Surv(Hospitalization.time,event) ~ Age.Group3 + Gender + Income.Region3, Survival)
TableS7<-summary(Coxph_Total)
TableS7

# Table 2, estimates of Cox regression for symptomatic patients
# Mediation effect model
Coxph_Symptomatic <- coxph(Surv(Hospitalization.time,event) ~ Age.Group3 + Gender + Income.Region3 + Grouped.Delay.Time.2, data =  Survival2)
Table2_Mediation<-summary(Coxph_Symptomatic)
Table2_Mediation

# Full effect model
Coxph_Symptomatic_full <- coxph(Surv(Hospitalization.time,event) ~ Age.Group3 + Gender + Income.Region3, data =  Survival2)
Table2_TotalEffect<-summary(Coxph_Symptomatic_full)
Table2_TotalEffect

# Table S4, estimates of logistic regression against severe reporting delay
med.model2 <- glm(Grouped.Delay.Time.2 ~ Age.Group3 + Gender + Income.Region3, family = binomial("logit"), data = Survival2)
summary(med.model2)
exp(summary(med.model2)$coefficients[,1])
exp(confint(med.model2))

# Table S3, hazard assumption of Cox regression
TableS3<- cox.zph(Coxph_Symptomatic)
TableS3
