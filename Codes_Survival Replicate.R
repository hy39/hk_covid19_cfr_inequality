library(mediation)
library(foreign)
library(survival)

Covid2_new<-read.csv("Data/New data_cleaned_updated.csv")

Covid2_new$Age_bin<-factor(Covid2_new$Age_bin,levels=c("0","1"))
Covid2_new$Gender<-factor(Covid2_new$Gender,levels=c("0","1"))
Covid2_new$Inc_bin_low<-factor(Covid2_new$Inc_bin_low,levels=c("0","1"))

# See Table S4 
med.model <- glm(GroupedDelayTime2 ~ Age_bin + Gender + Inc_bin_low, family = binomial(link = "logit"), data = Covid2_new)

summary(med.model)
exp(summary(med.model)$coefficients[,1])
exp(confint(med.model))
    
# See Table S5 Parameter estimates of survival regression against survival time
out.model <- survreg(Surv(Hospitalizationtime,event) ~ GroupedDelayTime2 + Age_bin + Gender + Inc_bin_low, dist='weibull', data =  Covid2_new)

summary(out.model)
exp(summary(out.model)$coefficients)
exp(confint(out.model))

set.seed(1234)

result_age <- mediate(med.model, out.model, treat="Age_bin", mediator="GroupedDelayTime2")
summary(result_age)

# Show more decimal digits
trace(mediation:::print.summary.mediate, 
      at = 11,
      tracer = quote({
        printCoefmat <- function(x, digits) {
          p <- x[, 4] #p-values seem to be stored rounded
          x[, 1:3] <- sprintf("%.6f", x[, 1:3])
          x[, 4] <- sprintf("%.6f", p)
          print(x, quote = FALSE, right = TRUE)
        } 
      }),
      print = FALSE)
mediation:::print.summary.mediate(summary(result_age))

set.seed(1234)

result_inc <- mediate(med.model, out.model, treat="Inc_bin_low", mediator="GroupedDelayTime2")

summary(result_inc)

mediation:::print.summary.mediate(summary(result_inc))
