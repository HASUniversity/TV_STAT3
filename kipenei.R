library(readxl)
library(tidyverse)
library(minpack.lm)
ei <- read_excel("../data/bestanden bij opgaven/kip_en_ei.xlsx")

ei %>% 
  ggplot(aes(hoeveelheid_sup, hardheid_schaal)) +
  geom_point() +
  theme_classic()

fit_lm <- lm(hardheid_schaal~hoeveelheid_sup, data=ei)
fit_lm_log <- lm(hardheid_schaal~log(hoeveelheid_sup), data=ei)
fit_nlm <- nlsLM(hardheid_schaal~a+b*log(hoeveelheid_sup), 
                    start=list(a=1, b=2), 
                    data=ei)
fit_lm$coefficients
coef(fit_lm_log)
coef(fit_nlm)

