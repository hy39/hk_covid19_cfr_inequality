---
Title: "HK COVID-19 CFR and Health Inequality"
Authors: "Wing Hei Wong, Fatema Khairunnasa, Hsiang-Yu Yuan"
Date: 07/11/2023
---

This repository contains the R codes used in the survival analysis of COVID-19 patients in Hong Kong in the project entitled with "The impact of economic and demographic conditions on COVID-19 diagnosis delays and deaths in Hong Kong".

To generate the required graphs, you will need to run all the packages/codes mentioned in the top 9 lines of code. You can run all the codes in the R script to generate the demographic figures used in the project. If there are such package, make sure to install these packages first (e.g. install.packages()).

##Survival Analysis

The Project Code_Survival Analysis.R, is divided into 2 parts. -K-M curves, -Cox regression model.

To produce the Kaplan-Meier (K-M) analysis of all covid-19 patients, first you need to run the following required libraries.

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
library(survival)
library(ggplot2)
library(plyr)
library(ggpubr)
library(data.table)
library(car)
library(survminer)
library(dplyr)
library(lubridate)
```

Then you have to run the following data sets for the survival analysis.

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
Covid<-read.csv("data/Covid model.csv")
Covid2<-read.csv("data/Covid model_symptomatic.csv")
```

#Subset Data for survival

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
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
```

Then run the following section to reproduce summary results of the survival analysis and the K-M plot in total patients.

#Kaplan-Meier (K-M) non-parametric analysis of all Covid-19 patients#

```{r message=FALSE, warning=FALSE}
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
```

Now to get the K-M survival analysis by age, gender and income region groups, run the following statements:

##Kaplan-Meier (K-M) non-parametric analysis by group## #Gender group

```{r message=FALSE, warning=FALSE}
KMSurvival_Gender <- survfit(Surv(Hospitalization.time,event) ~ Gender, data=Survival)
summary(KMSurvival_Gender)
KMPlot_Gender<-ggsurvplot(KMSurvival_Gender,
                          xlab= "", ylab= "Survival probability",
                          legend.title="Gender", pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                          conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 12, face = "bold"),
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
```

#Age group

```{r message=FALSE, warning=FALSE}
KMSurvival_Age <- survfit(Surv(Hospitalization.time,event) ~ Age.Group, data=Survival)
summary(KMSurvival_Age)
KMPlot_Age<-ggsurvplot(KMSurvival_Age,
                       xlab= "Day", ylab= "Survival probability",
                       legend.title="Age",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                       conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 12, face = "bold"),
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
```

#Income Regions Group

```{r message=FALSE, warning=FALSE}
KMSurvival_Income <- survfit(Surv(Hospitalization.time,event) ~ Income.Region, data=Survival)
summary(KMSurvival_Income)
KMPlot_Income<-ggsurvplot(KMSurvival_Income,
                          xlab= "Day", ylab= "Survival probability",
                          legend.title="Income region",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                          conf.int = F, conf.int.alpha= 0.2, font.legend = list(size = 12, face = "bold"),
                          legend.labs= c("High income region","Low income region","Middle income region"),
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
```

You can also reproduce the combined graph of above three for the comparison purpose.

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
Figure_S9<-ggarrange(KMPlot,KMPlot_Gender,KMPlot_Age,KMPlot_Income,
                    labels = c("A","B","C","D"), font.label = list(size = 12),
                    ncol = 2, nrow = 2)
Figure_S9
```

Now for the Kaplan-Meier analysis of the symptomatic patients run the following sections.

#Kaplan-Meier (K-M) non-parametric analysis of Symptomatic Patients

```{r message=FALSE, warning=FALSE}
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
```

You can also reproduce the individual survival summary and plots for different age, gender and income group in symptomatic patients using the section below.

##Kaplan-Meier (K-M) non-parametric analysis by group## #Gender group

```{r message=FALSE, warning=FALSE}
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
```

#Age group

```{r message=FALSE, warning=FALSE}
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
```

#Income Regions Group

```{r message=FALSE, warning=FALSE}
KMSurvival2_Income <- survfit(Surv(Hospitalization.time,event) ~ Income.Region, data=Survival2)
summary(KMSurvival2_Income)
KMPlot2_Income<-ggsurvplot(KMSurvival2_Income,
                           xlab= "Day", ylab= "Survival probability",
                           legend.title="Income region",pval = TRUE, pval.coord = c(1, 0.55), pval.size= 8,
                           legend.labs= c("High income region","Low income region","Middle income region"),
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
```

Using the following statement you can generate the combined graphs of the above three groups for the comparative study

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
FigureS7<-ggarrange(KMPlot2,KMPlot2_Gender,KMPlot2_Age,KMPlot2_Income,
                   labels = c("A","B","C","D"), font.label = list(size = 19),
                   ncol = 2, nrow = 2)
FigureS7
```

##Cox regression model# For the other part of survival analysis, run the following sections.

#Cox proportional hazard model & Assumption#

```{r}
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
```
