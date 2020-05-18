library(survival)
library(tidyverse)


#Dataset laden
df <- lung

#survivalobject maken, status=2 betekent overleden
S <- Surv(time=df$time, event=df$status, type="right")

#Kaplan-Meier survival curve
fit <- survfit(S~1)
fit
names(fit)

#Via plot
plot(fit)

#via ggplot
library(ggfortify)


#Effect van geslacht
fit <- survfit(S~df$sex)
autoplot(fit) +
  guides(fill=FALSE) +
  scale_color_manual(name = "Geslacht", labels = c("man", "vrouw"), values = c(1,2)) +
  theme_classic()


#Testen
survdiff(S~df$sex)

#Cox proportional hazard model
fit_cox <- coxph(S~df$sex)
fit_cox

#Complexer model
fit_cox <- coxph(S~df$sex+df$age+df$ph.karno)
summary(fit_cox)

plot(survfit(S~df$sex+df$age+df$ph.karno))
