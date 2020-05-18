library(lmPerm)
library(readxl)
library(car)

#kleine dataset
behandeling <- rep(c("controle", "bemest"), each = 5)
plantlengte <- c(15,17,16,14,16,15,23,21,27,18)
plantdata <- data.frame(behandeling, plantlengte)


fit_glm <- lm(plantlengte~behandeling, data=plantdata)
fit_perm <- lmp(plantlengte~behandeling, data=plantdata)

summary(fit_glm)
summary(fit_perm)

#Grote dataset
cijfers <- read_excel("../data/cijfers.xlsx")

fit_glm <- lm(biocalculus~geslacht, data=cijfers)
fit_perm <- lmp(biocalculus~geslacht, data=cijfers)

summary(fit_glm)
summary(fit_perm)

#hongerige krekels
df <- read_excel("../data/honger.xlsx")

fit_glm <- lm(tijdtotparing~Behandeling, data=df)
fit_perm <- lmp(tijdtotparing~Behandeling, data=df)

summary(fit_glm)
summary(fit_perm)


#Geboortegewicht zeugen

df <- read_excel("../data/zeug.xlsx")

fit <- lm(geboortegewicht~gewichtstoename_zeug, data = df)
plot(df$gewichtstoename_zeug, rstudent(fit))

#
roach <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f5_4CockroachNeurons.csv"))

library(writexl)
write_xlsx(roach, "../data/kakkerlak.xlsx")

kakkerlak <- read_excel("../data/kakkerlak.xlsx")
View(kakkerlak)
fit <- lm(rate~temperature, data = kakkerlak)
plot(kakkerlak$temperature, rstudent(fit))

fitp <- lmp(rate~temperature, data = kakkerlak)
summary(fit)
summary(fitp)


#Simulatie
getdf <- function(mu1, mu2, s1, s2, n) {
  t1 <- rnorm(n, mu1, s1)
  t2 <- rnorm(n, mu2, s2)
  return(data.frame(treat=rep(c("a", "b"), each=n),
                    score=c(t1, t2)))
}
pperm <- function(df) {
  fit <- summary(lmp(score~treat, data=df))
  results <- fit$fstatistic
  p <- pf(results[1], results[2], results[3], lower.tail = FALSE)
#  p <- as.numeric(p)
  return(p)
}

plm <- function(df) {
  fit <- summary(lm(score~treat, data=df))
  results <- fit$fstatistic
  p <- pf(results[1], results[2], results[3], lower.tail = FALSE)
  return(p)
}

plm_white <- function(df) {
  fit <- Anova(lm(score~treat, data=df), white.adjust=TRUE)
  p <- fit[1,3]
  return(p)
}

plm_welch <- function(df) {
  fit <- oneway.test(score~treat, data=df, var.equal = FALSE)
  return(fit$p.value)
}

pkrus <- function(df) {
  return(kruskal.test(score~treat, data=df)$p.value)  
}

#gelijke variantie
n=10
mu1=0
mu2=0
s1=5
s2=5
listpperm <- replicate(1000,pperm(getdf(mu1, mu2, s1, s2, n)))
hist(listpperm)
length(listpperm[listpperm<0.05])/10000
listplm <- replicate(10000, plm(getdf(mu1, mu2, s1, s2, n)))
hist(listplm)
length(listplm[listplm<0.05])/10000

#ongelijke variantie
n=10
mu1=0
mu2=0
s1=5
s2=1
listpperm <- replicate(1000,pperm(getdf(mu1, mu2, s1, s2, n)))
length(listpperm[listpperm<0.05])/10000*100
listplm <- replicate(10000, plm(getdf(mu1, mu2, s1, s2, n)))
length(listplm[listplm<0.05])/10000
listplwhite <- replicate(10000, plm_white(getdf(mu1, mu2, s1, s2, n)))
length(listplwhite[listplwhite<0.05])/10000
listplwelch <- replicate(10000, plm_welch(getdf(mu1, mu2, s1, s2, n)))
length(listplwelch[listplwelch<0.05])/10000
listpkrus <- replicate(10000, pkrus(getdf(mu1, mu2, s1, s2, n)))
length(listpkrus[listpkrus<0.05])/10000
