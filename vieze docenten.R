library(tidyverse)
library(readxl)

df <- read_excel("../data/swap.xlsx")
swapmean <- mean(df$aantal_kolonies)


df %>%  ggplot(aes(opleiding, aantal_kolonies)) +  
  geom_boxplot() + 
  theme_classic()

df %>%  ggplot(aes(geslacht, aantal_kolonies)) +  
  geom_boxplot() + 
  theme_classic()

df %>%  
  filter(opleiding != "Controle") %>% 
  ggplot(aes(opleiding, aantal_kolonies, color = geslacht)) +  
  geom_boxplot() + 
  geom_hline(yintercept = swapmean) +
  theme_classic()


df %>%  
  filter(opleiding != "Controle") %>% 
  ggplot(aes(fte, aantal_kolonies, color = geslacht)) +  
  geom_point() + 
  theme_classic()

df %>%  
  filter(opleiding != "Controle") %>% 
  ggplot(aes(specialisatie, aantal_kolonies, color = geslacht)) +  
  geom_boxplot() + 
  geom_hline(yintercept = swapmean) +
  theme_classic()

df %>%  
  filter(opleiding != "Controle") %>% 
  ggplot(aes(geslacht, aantal_kolonies)) +  
  geom_boxplot() + 
  geom_hline(yintercept = swapmean, linetype=2) +
  theme_classic()
