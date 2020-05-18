library(tidyverse)
library(minpack.lm)

Afstand <- c(1, 1.5, 2, 2.5, 3, 3.5)
Zink <- c(644, 612, 580, 565, 553, 549)
df <- data.frame(Afstand, Zink)

df %>% 
  ggplot(aes(Afstand, Zink)) + 
  geom_point() +
  theme_classic()

