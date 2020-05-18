library(readxl)
library(tidyverse)
library(nlme)
library(emmeans)
alcohol <- read_excel("../data/alcohol.xlsx")

alcohol_long <- alcohol %>%
  gather(key = "aantal", value = "score", -persoon) %>% 
  mutate(persoon = factor(persoon)) %>% 
  mutate(aantal = factor(aantal, 
                         levels = c("geen", "drie", "zes", "negen")))

fit <- lme(score~aantal, random = ~1 | persoon ,data=alcohol_long)
emmeans(fit, list(pairwise~aantal), adjust = "tukey")

anova.lme(fit)

