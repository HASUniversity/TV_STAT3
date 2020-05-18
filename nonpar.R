library(tidyverse)
library(readxl)

cijfers <- read_excel("../data/cijfers.xlsx")

#one sample
wilcox.test(cijfers$biocalculus, mu=6, alternative = "greater")
t.test(cijfers$biocalculus, mu=6, alternative = "greater")


#ongepaard
wilcox.test(cijfers$biocalculus~cijfers$geslacht)
t.test(cijfers$biocalculus~cijfers$geslacht)

#gepaard
wilcox.test(cijfers$biocalculus, cijfers$biologie, paired = TRUE)
t.test(cijfers$biocalculus, cijfers$biologie, paired = TRUE)
