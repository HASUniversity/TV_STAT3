library(survival)
library(tidyverse)
library(ggfortify)
library(survminer)

df <- rats
S <- Surv(df$time, df$status)

fit <- survfit(S~df$sex)
fit

autoplot(fit) + theme_classic()
plot(fit)
survdiff(S~df$sex)

fit2 <- survfit(S~df$sex+df$rx)
summary(fit2)
fit_cox <- coxph(S~df$sex+df$rx)
summary(fit_cox)

ggforest(fit_cox)
ggsurvplot(fit2, data=df)

surv_summary(fit2, data=df)
plot.cox.zph(fit2)
cox.zph(fit_cox)
