if(!require(ggplot2)){
 install.packages("ggplot2")
}
if(!require(plyr)){
  install.packages("plyr")
}
if(!require(ggpubr)){
  install.packages("ggpubr")
}
if(!require(data.table)){
  install.packages("data.table")
}
if(!require(carData)){
  install.packages("carData")
}
if(!require(car)){
  install.packages("car")
}
if(!require(pscl)){
  install.packages("pscl")
}
if(!require(ggrepel)){
  install.packages("ggrepel")
}
if(!require(patchwork)){
  install.packages("patchwork")
}
if(!require(reshape2)){
  install.packages("reshape2")
}
if(!require(tidyr)){
  install.packages("tidyr")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(lubridate)){
  install.packages("lubridate")
}
if(!require(reshape2)){
  install.packages("reshape2")
}
if(!require(scales)){
  install.packages("scales")
}
if(!require(forcats)){
  install.packages("forcats")
}
if(!require(corrplot)){
  install.packages("corrplot")
}
if(!require(Hmisc)){
  install.packages("Hmisc")
}

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

#District & Territory Demographic
DD<-read.csv("data/dis_inc_cfr.csv")
DS<-read.csv("data/dis_inc_cfr_symptomatic.csv")
TS<-read.csv("data/Territory_symptomatic.csv")
Covid2<-read.csv("data/Covid model_symptomatic.csv")

#Data Management
Covid2_Elderly<-Covid2%>%group_by(Age)%>%filter(any(Age>64))
Covid2_Elderly_Death<-Covid2_Elderly%>%group_by(event)%>%filter(any(event=="1"))
Covid2_Elderly_Cases.in.district<-Covid2_Elderly%>%group_by(district)%>%dplyr::summarise(Total=n()) # use dplyr::summarise
Covid2_Elderly_Death.in.district<-Covid2_Elderly_Death%>%group_by(district)%>%dplyr::summarise(Death=n())
new_row1 = list("Tai Po",0)
new_row2 = list(district="Sai Kung",Death=0)
Covid2_Elderly_Death.in.district = rbind(Covid2_Elderly_Death.in.district,new_row1)
Covid2_Elderly_Death.in.district = rbind(Covid2_Elderly_Death.in.district,new_row2)
Covid2_Elderly.in.district=left_join(Covid2_Elderly_Cases.in.district, Covid2_Elderly_Death.in.district)
Covid2_Elderly.in.district<-Covid2_Elderly.in.district%>%mutate(CFR_elderly=round(Death/Total*100,2))
colnames(Covid2_Elderly.in.district)[c(1)]<-c("District")
CFR_Overview=left_join(Covid2_Elderly.in.district, DS)
CFR_Overview<-subset(CFR_Overview,select =c(District,CFR_elderly,cfr_percent,income_trans,population_density))

#Sampling Distribution
Latest<-read.csv("data/Latest Situation.csv")

Latest$Report.date<-dmy(Latest$Report.date)
Latest<-Latest%>% mutate(Daily= Number.of.confirmed.cases - lag(Number.of.confirmed.cases, default = 0))
Latest<-Latest%>% mutate(Death= Number.of.death.cases - lag(Number.of.death.cases, default = 0))

Figure1B<-ggplot(Latest, aes(x=Report.date)) +
  geom_line(aes(y=Daily),size=1,color="red") + 
  geom_bar(aes(y=Death*30),width = 0.01,stat = "identity",color="blue") +  
  scale_x_date(date_labels = "%Y-%m", 
               breaks = seq(as.Date("2020-02-1"), as.Date("2021-10-1"), by="6 month"),expand = c(0,0),limits = c(as.Date("2020-01-23"), as.Date("2021-10-15"))) + 
  labs(x="") + 
  theme(axis.text.x=element_text(hjust=0.5)) +
  scale_y_continuous(name="Daily cases",expand=c(0,0),limits = c(-5, 155),breaks = seq(0, 150, 30), 
                     sec.axis = sec_axis(~./30,name = "Daily Deaths")) +   
  theme(axis.text.x=element_text(size=18,color = "#363636"),
        axis.text.y=element_text(size=18),
        axis.title = element_text(size = 18,color="red"),
        axis.title.y.right = element_text(size=18, color = "blue"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks = element_line(size = 1.05))
Figure1B

#Scatter Plot_Total#
Scatter_Total_CI_I<-ggplot(DD,aes(x=Income_1k, y=Cumulative.Incidence.Rate_percentage, size =population_density, label=District)) + 
  geom_vline(xintercept=c(mean(DD$Income_1k)), linetype="dashed",lwd=1) + 
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Median monthly income \n (HKD$ '000)") + ylab("Cumulative incidence \n (cases per 1000 people)") + labs(size="Population density")
Scatter_Total_CI_I<-Scatter_Total_CI_I + scale_x_continuous(expand= c(0,0), limits = c(20,46),breaks = seq(20, 46, 4)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.38), breaks= seq(0,0.35,0.05)) + labs(size="Population density")+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12), legend.position = c(0.85,0.84)) 
Scatter_Total_CI_I

Scatter_Total_CFR_I<-ggplot(DD,aes(x=Income_1k, y=cfr_percent, size =population_density, label=District)) + 
  geom_vline(xintercept=c(mean(DD$Income_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Median monthly income \n (HKD$ '000)") + ylab("Case fatality rate (%)")
Scatter_Total_CFR_I<-Scatter_Total_CFR_I + scale_x_continuous(expand= c(0,0), limits = c(20,46),breaks = seq(20, 46, 4)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,4.5), breaks= seq(0,4.5,0.5)) + labs(size="Population density")+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12), legend.position = c(0.85,0.835)) 
Scatter_Total_CFR_I

Scatter_Total_CI_PD<-ggplot(DD,aes(x=population.density_1k, y=Cumulative.Incidence.Rate_percentage, label=District))+ 
  geom_vline(xintercept=c(mean(DD$population.density_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Population density \n (1000 person per sq. km)") + ylab("Cumulative incidence \n (cases per 1000 people)")
Scatter_Total_CI_PD<-Scatter_Total_CI_PD + scale_x_continuous(expand= c(0,0), limits = c(0,65),breaks = seq(0, 60, 10)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.38), breaks= seq(0,0.35,0.05))+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12))
Scatter_Total_CI_PD

Scatter_Total_CFR_PD<-ggplot(DD,aes(x=population.density_1k, y=cfr_percent, label=District)) + 
  geom_vline(xintercept=c(mean(DD$population.density_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Population density \n (1000 person per sq. km)") + ylab("Case fatality rate (%)")
Scatter_Total_CFR_PD<-Scatter_Total_CFR_PD + scale_x_continuous(expand= c(0,0), limits = c(0,65),breaks = seq(0, 60, 10)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,4.5), breaks= seq(0,4.5,0.5))+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12))
Scatter_Total_CFR_PD

FigureS6<-ggarrange(Scatter_Total_CI_I,Scatter_Total_CFR_I,Scatter_Total_CI_PD,Scatter_Total_CFR_PD,
                    labels = c("A","B","C","D"),font.label = list(size = 12),
                    ncol = 2, nrow = 2)
FigureS6

#Scatter Plot_Symptomatic#
Scatter_Symptomatic_CI_I<-ggplot(DS,aes(x=income_trans, y=inc_perk, size =population_density, label=District)) + 
  geom_vline(xintercept=c(mean(DD$Income_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Median monthly income \n (HKD$ '000)") + ylab("Cumulative incidence \n (cases per 1000 people)") + labs(size="Population density")
Scatter_Symptomatic_CI_I<-Scatter_Symptomatic_CI_I + scale_x_continuous(expand= c(0,0), limits = c(20,46),breaks = seq(20, 46, 4)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,2.5), breaks= seq(0,2.5,0.5)) + labs(size="Population density")+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12), legend.position = c(0.8,0.84),
        legend.title = element_text(size=12),legend.text = element_text(size=12))
Scatter_Symptomatic_CI_I

Scatter_Symptomatic_CFR_I<-ggplot(DS,aes(x=income_trans, y=cfr_percent, size =population_density, label=District)) + 
  geom_vline(xintercept=c(mean(DD$Income_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7) +
  xlab("Median monthly income \n (HKD$ '000)") + ylab("Case fatality rate (%)")
Scatter_Symptomatic_CFR_I<-Scatter_Symptomatic_CFR_I + scale_x_continuous(expand= c(0,0), limits = c(20,46),breaks = seq(20, 46, 4)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,4.5), breaks= seq(0,4.5,0.5)) + labs(size="Population density")+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12), legend.position = c(0.8,0.835),
        legend.title = element_text(size=12),legend.text = element_text(size=12))
Scatter_Symptomatic_CFR_I

Scatter_Symptomatic_CI_PD<-ggplot(DS,aes(x=population_density_1000, y=inc_perk, label=District)) + 
  geom_vline(xintercept=c(mean(DD$population.density_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7) +
  xlab("Population density \n (1000 Person per sq. km)") + ylab("Cumulative incidence \n (cases per 1000 people)")
Scatter_Symptomatic_CI_PD<-Scatter_Symptomatic_CI_PD + scale_x_continuous(expand= c(0,0), limits = c(0,65),breaks = seq(0, 60, 10)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,2.5), breaks= seq(0,2.5,0.5))+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12))
Scatter_Symptomatic_CI_PD

Scatter_Symptomatic_CFR_PD<-ggplot(DS,aes(x=population_density_1000, y=cfr_percent, label=District)) + 
  geom_vline(xintercept=c(mean(DD$population.density_1k)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 3,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7)+
  xlab("Population density \n 1000 Person per sq. km)") + ylab("Case fatality rate (%)")
Scatter_Symptomatic_CFR_PD<-Scatter_Symptomatic_CFR_PD + scale_x_continuous(expand= c(0,0), limits = c(0,65),breaks = seq(0, 60, 10)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,4.5), breaks= seq(0,4.5,0.5))+
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 12))
Scatter_Symptomatic_CFR_PD

Figure2<-ggarrange(Scatter_Symptomatic_CI_I,Scatter_Symptomatic_CFR_I,Scatter_Symptomatic_CI_PD,Scatter_Symptomatic_CFR_PD,
                   labels = c("A","B","C","D"), font.label = list(size = 12),
                   ncol = 2, nrow = 2)
Figure2

Scatter_CFR_Comparsion<-ggplot(CFR_Overview,aes(x=cfr_percent, y=CFR_elderly, label=District)) + 
  geom_label_repel(size = 3.5,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7,size=5)+
  xlab("Case fatality rate \n in symptomatic patients (%)") + ylab(" Case fatality rate \n in elderly (%)") + 
  scale_x_continuous(expand= c(0,0), limits = c(0,4.5),breaks = seq(0, 4, 1)) +
  scale_y_continuous(expand=c(0,0.1), limits=c(0,18), breaks= seq(0,18,2))+
  theme(axis.text=element_text(size=18), axis.title = element_text(size = 18)) 
Scatter_CFR_Comparsion

Scatter_Symptomatic_Elderly_CFR_I<-ggplot(CFR_Overview,aes(x=income_trans, y=CFR_elderly, size =population_density, label=District)) + 
  geom_vline(xintercept=c(mean(CFR_Overview$income_trans)), linetype="dashed",lwd=1) +
  geom_label_repel(size = 8.5,min.segment.length = 0, box.padding = 0.5)+ geom_point(alpha=0.7) +
  xlab("Median monthly income \n (HKD$ '000)") + ylab("Case fatality rate in elderly (%)") + 
  scale_x_continuous(expand= c(0,0), limits = c(20,46),breaks = seq(20, 46, 4)) +
  scale_y_continuous(expand=c(0,0.1), limits=c(0,20), breaks= seq(0,20,2)) + 
  labs(size="Population density")+
  theme(axis.text=element_text(size=18), axis.title = element_text(size = 18), 
        legend.position = c(0.8,0.835), legend.title = element_text(size=18), 
        legend.text = element_text(size=18))
Scatter_Symptomatic_Elderly_CFR_I

FigureS2<-ggarrange(Scatter_Symptomatic_Elderly_CFR_I, Scatter_CFR_Comparsion,
                    labels = c("A","B"), font.label = list(size = 18),
                    nrow = 1)
FigureS2

#Bar Chart_Symptomatic
level_order_TS<- factor(TS$Territory, level = c("KLE", "KLW", "NTW","HK","NTE"))

TS_CFR<-ggplot(TS, aes(x = level_order_TS, cfr,2, label=cfr)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Case fatality rate") +
  xlab("Territories") +
  ylab("Percentage (%)") +
  geom_text(size = 5, position = position_stack(vjust =0.95), color="white") +
  scale_y_continuous(expand= c(0,0),breaks = seq(0, 3.5, 0.5)) + coord_cartesian(ylim = c(0, 3.5)) +
  theme(axis.text=element_text(size=16), axis.title = element_text(size = 15),plot.title = element_text(size = 15))
TS_CFR

TS_CDP<-ggplot(TS, aes(x = level_order_TS, y = CDPinPop_percent, label=CDPinPop_percent)) + 
  geom_bar(stat = "identity") +
  ggtitle("People with chronic disease") + ylab("Percentage (%)") +
  xlab("Territories") +
  geom_text(size = 5, position = position_stack(vjust =0.98), color="white") +
  scale_y_continuous(expand= c(0,0),breaks = seq(0, 30, 5)) + coord_cartesian(ylim = c(0, 30)) +
  theme(axis.text=element_text(size=16), axis.title = element_text(size = 15),plot.title = element_text(size = 15))
TS_CDP

FigureS3<-ggarrange(TS_CFR,TS_CDP,
                    labels = c("A","B"),
                    ncol = 2)
FigureS3

#correlation between CFR and CDPinPop_percent
cor(TS$cfr,TS$CDPinPop_percent,method="pearson")
cor.test(TS$cfr,TS$CDPinPop_percent)

#CFR among reporting delay & hospitalization time
Covid2<-read.csv("data/Covid model_symptomatic.csv")
Covid2_Delay<-read.csv("data/DeathRisk.csv")
Covid2_Delay2<-read.csv("data/DeathRisk_Individual.csv")
Covid2_agepercent<-read.csv("data/Covid model_symptomatic_2Income.csv")
Covid2_agepercent2<-read.csv("data/Covid model_symptomatic_70_2Income.csv")

Covid2$Income.Region<-factor(Covid2$Income.Region,levels=c("Low Income Region","Middle Income Region","High Income Region"))
Covid2$Grouped.Delay.Time <- factor(Covid2$Grouped.Delay.Time, level = c("0", "1", "2","3","4","5","6","7","8","9","10",">10"))

Covid2_agepercent$Income.Region<-factor(Covid2_agepercent$Income.Region,levels=c("High Income Region","Low and Middle Income Region"))
Covid2_agepercent$Gender.Age<-factor(Covid2_agepercent$Gender.Age,levels=c("Male(≤64 years old)","Female(≤64 years old)","Male(>64 years old)","Female(>64 years old)"))

Covid2_agepercent2$Income.Region<-factor(Covid2_agepercent2$Income.Region,levels=c("High Income Region","Low and Middle Income Region"))
Covid2_agepercent2$Gender.Age<-factor(Covid2_agepercent2$Gender.Age,levels=c("Male(≤69 years old)","Female(≤69 years old)","Male(>69 years old)","Female(>69 years old)"))

#Subset Data
Covid2_65<-Covid2 %>% filter(is.na(Age) | Age >= 65)
Covid2_65$Income.Region<-factor(Covid2_65$Income.Region,levels=c("Low Income Region","Middle Income Region","High Income Region"))

Delay_Total_LMH<-Covid2 %>% group_by(Income.Region)%>%count(Grouped.Delay.Time)
Delay_Total_LMH<-Delay_Total_LMH %>% group_by(Income.Region) %>% mutate(percentage=n/sum(n)*100)

Delay_Elderly_LMH<-Covid2_65 %>% group_by(Income.Region) %>% count(Grouped.Delay.Time)
Delay_Elderly_LMH<-Delay_Elderly_LMH %>% group_by(Income.Region) %>% mutate(percentage=n/sum(n)*100)
Delay <- data.frame(Income.Region=c("High Income Region"),Grouped.Delay.Time=c(0),n=c(0),percentage=c(0))
Delay$Grouped.Delay.Time<-as.factor(Delay$Grouped.Delay.Time)
Delay_Elderly_LMH<-rbind(Delay_Elderly_LMH, Delay)
remove(Delay)

Covid2_Alive<-Covid2%>%group_by(Income.Region,event)%>%filter(any(event == "0"))%>% count(Hospitalization.time)%>% mutate(percentage=n/sum(n)*100)
Covid2_Dead<-Covid2%>%group_by(Income.Region,event)%>%filter(any(event == "1"))%>% count(Hospitalization.time)%>% mutate(percentage=n/sum(n)*100)

#CFR over delay time
Covid2_Delay$Delay.Time <- factor(Covid2_Delay$Delay.Time, level = c(">1",">2",">3",">4",">5",">6",">7",">8",">9",">10",">11",">12"))
CFR_delay<-ggplot(Covid2_Delay,aes(x=Delay.Time, y=cfr_percent, group=1)) + geom_point(size = 3) + geom_line(size = 2) + 
  xlab("Reporting delay (days)") + ylab("Case fatality rate (%)") +
  scale_y_continuous(expand=c(0,0), limits=c(0,4.2), breaks= seq(0,4,1)) +
  theme(axis.text=element_text(size=18), axis.title = element_text(size = 18))
CFR_delay

Covid2_Delay2$Delay.time <- as.factor(Covid2_Delay2$Delay.time)
CFR_delay2<-ggplot(Covid2_Delay2,aes(x=Delay.time, y=CFR_3days,group=1)) + geom_point(size = 3) + geom_line(size = 2) + 
  xlab("Reporting delay (days)") + ylab("Case fatality rate (%)") + ggtitle("") +
  scale_y_continuous(expand=c(0,0), limits=c(0,3.2), breaks= seq(0,3,1)) +
  theme(axis.text=element_text(size= 18), axis.title = element_text(size = 18))
CFR_delay2

Figure3AB<-ggarrange(CFR_delay, CFR_delay2,
                   labels = c("A","B"),font.label = list(size = 18),
                   ncol = 1)
Figure3AB

#Proportion of delayed reporting time of symptomatic patients
level_order_Delay_Total  <- factor(Delay_Total_LMH$Grouped.Delay.Time, level = c("0", "1", "2","3","4","5","6","7","8","9","10",">10"))
Bar_Delay_Total_LMH<-ggplot(Delay_Total_LMH, aes(x=level_order_Delay_Total , y=percentage, label=percentage, fill = Income.Region, group = Income.Region)) + geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Symptomatic patients") + xlab("Delay time (Days)") + 
  ylab("Percentage with \n different delay time") +
  scale_y_continuous(expand=c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + labs(fill="Income Region") +
  theme(plot.title = element_text(size=24), axis.text=element_text(size=23), axis.title = element_text(size = 24),
        legend.position = c(0.77,0.74), legend.key.size = unit(2,"cm"),legend.title = element_text(size=20),legend.text = element_text(size=18))
Bar_Delay_Total_LMH

#Proportion of delayed reporting time of elderly patients
Delay_Elderly_LMH$Income.Region<-factor(Delay_Elderly_LMH$Income.Region,levels=c("Low Income Region","Middle Income Region","High Income Region"))
level_order_Delay_Elderly <- factor(Delay_Elderly_LMH$Grouped.Delay.Time, level = c("0", "1", "2","3","4","5","6","7","8","9","10",">10"))

Bar_Delay_Elderly_LMH<-ggplot(Delay_Elderly_LMH, aes(x=level_order_Delay_Elderly , y=percentage, label=percentage, fill = Income.Region, group = Income.Region)) + geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Symptomatic elderly patients") + xlab("Delay time (Days)") + ylab("Percentage with \n different delay time") +
  scale_y_continuous(expand=c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + labs(fill="Income Region") +
  theme(plot.title = element_text(size=12), axis.text=element_text(size=12), axis.title = element_text(size = 12), 
        legend.position = c(0.77,0.74), legend.key.size = unit(2,"cm"),legend.title = element_text(size=20),legend.text = element_text(size=18))
Bar_Delay_Elderly_LMH

#Percentage of severe delay patients
my.labels <- c("Low and middle \n income region",
               "High \n income region")
Bar_Percentage<-ggplot(Covid2_agepercent, aes(x= fct_rev(Income.Region), y= Percentage_65, fill = fct_rev(Gender.Age))) + 
  ggtitle("Symptomatic patients") + xlab("") + coord_flip() +
  ylab("Percentage with \n severe reporting delay (>10 days)") + scale_x_discrete(labels= my.labels) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  scale_fill_viridis_d(breaks = rev, direction = -1) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 13), breaks = seq(0, 12, 2)) + labs(fill="Income Region") +
  theme(plot.title = element_text(size=12), axis.text.x=element_text(size= 12), 
        axis.title = element_text(size = 12), axis.text.y =element_text(size= 12, angle = 90,vjust = 0.7, hjust=0.5,color = "#000000"),
        legend.position = c(0.8,0.8), legend.key.size = unit(1,"cm"),
        legend.title = element_text(size=12),legend.text = element_text(size=12)) +
  scale_fill_manual(values = c("Male(≤64 years old)" = "#ADD8E6","Female(≤64 years old)" = "#ffcccb","Male(>64 years old)" = "#00008B","Female(>64 years old)" = "#8b0000"))
Bar_Percentage

Bar_Percentage_70<-ggplot(Covid2_agepercent2, aes(x= fct_rev(Income.Region), y= Percentage, fill = fct_rev(Gender.Age))) + 
  ggtitle("Symptomatic patients") + xlab("") + coord_flip() +
  ylab("Percentage with \n severe reporting delay (>10 days)") + scale_x_discrete(labels= my.labels) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  scale_fill_viridis_d(breaks = rev, direction = -1) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 13), breaks = seq(0, 12, 2)) + labs(fill="Income Region") +
  theme(plot.title = element_text(size=24), axis.text.x=element_text(size= 22), 
        axis.title = element_text(size = 24), axis.text.y =element_text(size= 24, angle = 90,vjust = 0.7, hjust=0.5,color = "#000000"),
        legend.position = c(0.8,0.8), legend.key.size = unit(1,"cm"),
        legend.title = element_text(size=20),legend.text = element_text(size=18)) +
  scale_fill_manual(values = c("Male(≤69 years old)" = "#ADD8E6","Female(≤69 years old)" = "#ffcccb","Male(>69 years old)" = "#00008B","Female(>69 years old)" = "#8b0000"))
Bar_Percentage_70

Figure3CDE<-ggarrange(Bar_Delay_Total_LMH,Bar_Delay_Elderly_LMH,Bar_Percentage,
                   labels = c("C","D","E"),
                   ncol = 1,nrow=3)
Figure3CDE

#CFR over hospitalization time
Covid2_Alive.Hist<-ggplot(Covid2_Alive, aes(x=Hospitalization.time, y = percentage, label=percentage)) + 
  scale_x_continuous(expand= c(0,0), limits = c(0,100),breaks = seq(0, 100, 10)) +
  geom_bar(stat = "identity") + scale_y_continuous(expand= c(0,0), limits = c(0,8.5),breaks = seq(0, 10, 1)) + 
  facet_grid(factor(Income.Region, levels=c("Low Income Region", "Middle Income Region","High Income Region"))~.) + 
  labs(title="Alive group distribution",x="Hospitalization time (Days))", y = "Percentage (%)") + 
  theme(panel.spacing.y = unit(12, "pt"),strip.text.y = element_text(size = 12), 
        axis.text=element_text(size=8), axis.title = element_text(size = 12),plot.title = element_text(size = 12))
Covid2_Alive.Hist

Covid2_Dead.Hist<-ggplot(Covid2_Dead, aes(x=Hospitalization.time, y = percentage, label=percentage)) + 
  scale_x_continuous(expand= c(0,0), limits = c(0,100),breaks = seq(0, 100, 10)) +
  geom_bar(stat = "identity") + scale_y_continuous(expand= c(0,0), limits = c(0,30),breaks = seq(0, 30, 5)) + 
  facet_grid(factor(Income.Region, levels=c("Low Income Region", "Middle Income Region","High Income Region"))~.) + 
  labs(title="Dead group distribution",x="Hospitalization time (Days))", y = "Percentage (%)") + 
  theme(panel.spacing.y = unit(12, "pt"),strip.text.y = element_text(size = 12), 
        axis.text=element_text(size=8), axis.title = element_text(size = 12),plot.title = element_text(size = 12))
Covid2_Dead.Hist

FigureSA<-ggarrange(Covid2_Alive.Hist,Covid2_Dead.Hist,
                    labels = c("A","B"),font.label = list(size = 12),
                    ncol = 1, nrow = 2)
FigureSA

#Correlation
DD<-read.csv("data/dis_inc_cfr.csv")
DS<-read.csv("data/dis_inc_cfr_symptomatic.csv")
Covid<-read.csv("data/Covid model.csv")
Covid2<-read.csv("data/Covid model_symptomatic.csv")
Covid2_Dead<-Covid2%>%group_by(event)%>%filter(any(event=="1"))

#Matrix
df<-data.frame(DD$inc_perk,DD$cfr_percent,DD$population_density,DD$Income, DD$Average.Age,DD$Average.Patient.Age,DD$aht)
df2<-data.frame(DS$inc_perk,DS$cfr_percent,DS$population_density,DS$Income,DS$adt, DS$Average.Age,DS$Average.Patient.Age,DS$aht)
df3<-data.frame(Covid2$event,Covid2$Numeric.Gender,Covid2$Age,Covid2$Hospitalization.time,Covid2$Delay.Time)
df4<-data.frame(Covid2_Dead$Numeric.Gender,Covid2_Dead$Age,Covid2_Dead$Hospitalization.time,Covid2_Dead$Delay.Time)

#Correlation Function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#Correlation Matrix
M1<-cor(df)
colnames(M1) <- c("CI", "CFR", "Population Density", "Median Income", "Average Age", "Average Patient Age","Average Hospitalization Time")
rownames(M1) <- c("CI", "CFR", "Population Density", "Median Income", "Average Age", "Average Patient Age","Average Hospitalization Time")
head(M1)
p.mat <- cor.mtest(df)
colnames(p.mat) <- c("CI", "CFR", "Population Density", "Median Income", "Average Age", "Average Patient Age","Average Hospitalization Time")
rownames(p.mat) <- c("CI", "CFR", "Population Density", "Median Income", "Average Age", "Average Patient Age","Average Hospitalization Time")
head(p.mat)
FigureS1A<-corrplot(M1, method="color", tl.col="black", tl.srt=75, type="upper",p.mat = p.mat,
                    sig.level = 0.05, title = "Total patients",mar=c(0,0,1,0))

M2<-cor(df2)
colnames(M2) <- c("CI", "CFR", "Population Density", "Median Income", "Average Delay Time", "Average Age", "Average Patient Age", "Average Hospitalization Time")
rownames(M2) <- c("CI", "CFR", "Population Density", "Median Income", "Average Delay Time", "Average Age", "Average Patient Age", "Average Hospitalization Time")
head(M2)
p2.mat <- cor.mtest(df2)
colnames(p2.mat) <- c("CI", "CFR", "Population Density", "Median Income", "Average Delay Time", "Average Age", "Average Patient Age", "Average Hospitalization Time")
rownames(p2.mat) <- c("CI", "CFR", "Population Density", "Median Income", "Average Delay Time", "Average Age", "Average Patient Age", "Average Hospitalization Time")
head(p2.mat)
FigureS1B<-corrplot(M2, method="color", tl.col="black", tl.srt=75, type="upper",p.mat = p2.mat,
                    sig.level = 0.05, title = "Symptomatic patients",mar=c(0,0,1,0))

M3<-cor(df3)
head(M3)
colnames(M3) <- c("Deceased Condition", "Gender", "Age", "Hospitalization Time", "Report Delay Time")
rownames(M3) <- c("Deceased Condition", "Gender", "Age", "Hospitalization Time", "Report Delay Time")
p3.mat <- cor.mtest(df3)
colnames(p3.mat) <- c("Deceased Condition", "Gender", "Age", "Hospitalization Time", "Report Delay Time")
rownames(p3.mat) <- c("Deceased Condition", "Gender", "Age", "Hospitalization Time", "Report Delay Time")
head(p3.mat)
Figure3F<-corrplot(M3, method="color", tl.col="black", tl.srt=75, type="upper",p.mat = p3.mat,
                    sig.level = 0.05, title = "Symptomatic patients",mar=c(0,0,1,0))

M4<-cor(df4)
head(M4)
colnames(M4) <- c("Gender", "Age", "Hospitalization Time", "Report Delay Time")
rownames(M4) <- c("Gender", "Age", "Hospitalization Time", "Report Delay Time")
p4.mat <- cor.mtest(df4)
colnames(p4.mat) <- c("Gender", "Age", "Hospitalization Time", "Report Delay Time")
rownames(p4.mat) <- c("Gender", "Age", "Hospitalization Time", "Report Delay Time")
head(p4.mat)
FigureS4<-corrplot(M4, method="color", tl.col="black", tl.srt=75, type="upper",
                   p.mat = p4.mat, sig.level = 0.05,title = "Symptomatic Deceased patients",mar=c(0,0,1,0))



