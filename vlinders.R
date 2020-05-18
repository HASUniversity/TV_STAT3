library(tidyverse)
library(readxl)
library(nlme)
library(emmeans)

#Data ophalen:
df <- read_excel("../data/vlinders.xlsx")

#Data omzetten naar lange tabel
df_long <- df %>% gather(key="jaar", value = "aantal", -provincie, -ligging, -tuin)

#T-toets uitvoeren. 
#NB als een functie niet direct de data uit de pipe herkent, 
#kan je de output van de vorige regel oproepen met .
df_long %>% 
  filter(jaar == 2008) %>% 
  t.test(aantal~provincie, data = .)

#Jaar 2008 en 2009 vergelijken: gepaarde waarnemingen, dus gepaarde t-toets
#Met != filter je op waardes die NIET overeenkomen
df_long %>% 
  filter(jaar != 2010) %>% 
  t.test(aantal~jaar, paired = TRUE, data = .)


#Testen of ligging een effect heeft.
#Jaar en tuinnummer zijn random variabelen
fit <- lme(aantal~ligging*provincie, 
           random = list(~1|jaar, ~ 1|tuin),
           data = df_long)
anova(fit)
summary(fit)
emmeans(fit, list(pairwise ~ ligging), adjust = "tukey")

df_long %>% 
  ggplot(aes(jaar, aantal, colour = ligging, shape = provincie, group = tuin)) +
  geom_line() +
  geom_point() +
  theme_classic()
