#
# POL 212 - Winter 2024
# regression.R
#
library(car)
library(MASS)
library(haven)
library(dplyr)
library(interactions)
library(ggplot2)
library(ggcorrplot)
library(jtools)
#
# FWL DEMONSTRATION
#
N <- 300
age <- sample(18:80, N, replace=TRUE)
agecat <- ntile(age, n=5)
income <- rgamma(300, shape=18, rate=0.3) + 0.3*age
#
cor(age, income)
#
polknow <- 1 + 2*age + 0.5*income + rnorm(300, 0, 25)
#
ols.ageonly <- lm(polknow ~ age)
ols.incomeonly <- lm(polknow ~ income)
ols.both <- lm(polknow ~ age + income)
ols.ageonincome <- lm(age ~ income)
ols.incomeonage <- lm(income ~ age)
#
jtools::summ(ols.ageonly)
jtools::summ(ols.incomeonly)
jtools::summ(ols.both)
#
yhat.ageonly <- predict(ols.ageonly)
residuals.ageonly <- ols.ageonly$residuals
residuals.ageonly.check <- (polknow - yhat.ageonly)
#
plot(residuals.ageonly, residuals.ageonly.check)
#
residuals.incomeonly <- ols.incomeonly$residuals
residuals.both <- ols.both$residuals
residuals.ageonincome <- ols.ageonincome$residuals
residuals.incomeonage <- ols.incomeonage$residuals
#
#
# FWL (Frisch-Waugh-Lovell) steps:
#    1.) Regress polknow (y) on income (X2): "ols.incomeonly"
#    2.) Regress age (X1) on income (X2): "ols.ageonincome"
#    3.) Obtain residuals from #1 and #2: "residuals.incomeonly" and "residuals.ageonincome"
#    4.) Regress "residuals.incomeonly" on "residuals.ageonincome", as below:
#
ols.residualsincome <- lm(residuals.incomeonly ~ residuals.ageonincome)
jtools::summ(ols.residualsincome)
#
#
# MODELING 2020 VOTE CHOICE
#
ANES2020 <- read_dta("D:/Dropbox/Files/Davis/Winter 2024/POL 212/data/ANES2020.dta")
#
# Thermometers
#
biden.rating <- ANES2020$V201151
biden.rating[biden.rating < 0] <- NA
biden.rating[biden.rating > 100] <- NA
#
trump.rating <- ANES2020$V201152
trump.rating[trump.rating < 0] <- NA
trump.rating[trump.rating > 100] <- NA
#
candidate.difference <- trump.rating - biden.rating
#
ggplot(data.frame(candidate.difference), aes(x=candidate.difference)) +
  geom_histogram(fill="blue2")
#
ggplot(na.omit(data.frame(candidate.difference)), aes(x=candidate.difference)) +
  geom_histogram(fill="blue2", bins=40)
#
#
#  Party identification
#  1 = STRONG DEMOCRAT, 7 = STRONG REPUBLICAN
#
partyid <- ANES2020$V201231x
partyid[partyid < 1] <- NA
partyid[partyid > 7] <- NA
#
plotdf <- na.omit(data.frame(candidate.difference, partyid))
#
ggplot(plotdf, aes(x=partyid, y=candidate.difference)) +
  geom_point() +
  geom_smooth(method="loess") + geom_smooth(method="lm", col="red")
#
ggplot(plotdf, aes(x=partyid, y=candidate.difference)) +
  geom_jitter(alpha = 0.1) +  # note: alpha controls transparency
  geom_smooth(method="loess") + geom_smooth(method="lm", col="red")
#
#
#  Issues
#
libcon <- ANES2020$V201200
libcon[libcon < 1] <- NA
libcon[libcon > 7] <- NA
#
govtservices <- ANES2020$V201246
govtservices[govtservices < 1] <- NA
govtservices[govtservices > 7] <- NA
govtservices <- -1 * govtservices + 8
#
defensespending <- ANES2020$V201249
defensespending[defensespending < 1] <- NA
defensespending[defensespending > 7] <- NA
#
healthinsurance <- ANES2020$V201252
healthinsurance[healthinsurance < 1] <- NA
healthinsurance[healthinsurance > 7] <- NA
#
guarjobs <- ANES2020$V201255
guarjobs[guarjobs < 1] <- NA
guarjobs[guarjobs > 7] <- NA
#
abortion <- ANES2020$V201336
abortion[abortion < 1] <- NA
abortion[abortion > 4] <- NA
#
immigrationlevel <- ANES2020$V202232
immigrationlevel[immigrationlevel < 1] <- NA
immigrationlevel[immigrationlevel > 5] <- NA
#
cor(data.frame(libcon, govtservices, defensespending, healthinsurance, guarjobs, abortion, immigrationlevel))
#
cor(data.frame(libcon, govtservices, defensespending, healthinsurance, guarjobs, abortion, immigrationlevel), use="pair")
#
plotdf <- cor(data.frame(libcon, govtservices, defensespending, healthinsurance, guarjobs, abortion, immigrationlevel), use="pair")
#
ggcorrplot(plotdf, type="upper", lab=TRUE)
#
#
#  Political knowledge
#  1 = CORRECT, 0 = INCORRECT
#
libcon.demparty <- ANES2020$V201206
libcon.demparty[libcon.demparty < 1] <- NA
libcon.demparty[libcon.demparty > 7] <- NA
#
libcon.repparty <-ANES2020$V201207
libcon.repparty[libcon.repparty < 1] <- NA
libcon.repparty[libcon.repparty > 7] <- NA
#
correctplacement <- as.numeric(libcon.demparty < libcon.repparty)
correctplacement[is.na(correctplacement)] <- 0
#
plotdf <- na.omit(data.frame(candidate.difference, partyid, correctplacement))
#
ggplot(plotdf, aes(x=partyid, y=candidate.difference)) +
  facet_wrap(~ correctplacement) +
  geom_jitter(alpha = 0.1) +  # note: alpha controls transparency
  geom_smooth(method="loess")
#
#
# Race
#
nonhispwhite <- ANES2020$V201549x
nonhispwhite[ANES2020$V201549x==1] <- 1
nonhispwhite[ANES2020$V201549x!=1] <- 0
#
#
# ESTIMATE LINEAR REGRESSION MODEL
#
dat <- data.frame(y=candidate.difference, partyid, libcon, immigrationlevel, defensespending, nonhispwhite)
#
ols <- lm(y ~ ., data=dat)
summary(ols)
#
partyid.factor <- factor(partyid, labels=c("SD","WD","LD","I","LR","WR","SR"))
dat2 <- data.frame(y=candidate.difference, partyid=relevel(partyid.factor, ref="I"), libcon, immigrationlevel, defensespending, nonhispwhite)
dat2.a <- data.frame(y=candidate.difference, partyid.factor, libcon, immigrationlevel, defensespending, nonhispwhite)
#
ols2 <- lm(y ~ ., data=dat2)
summary(ols2)
#
ols2.a <- lm(y ~ ., data=dat2.a)
summary(ols2.a)
#
yhat <- predict(ols2.a)
cor(na.omit(dat2.a)$y, yhat)
cor(na.omit(dat2.a)$y, yhat)^2
#
#
export_summs(ols2,ols2.a)
#
# Standardized coefficients (importance of variable scales on interpretation)
#
# UPDATE THIS WITH JTOOLS
#plot_model(ols, type = "std")
#
dat.scaled <- apply(dat, 2, scale)
#
# Show marginal effects of single variable
#
immigrationlevel.numeric <- as.numeric(immigrationlevel)
dat3 <- data.frame(y=candidate.difference, partyid, libcon, immigrationlevel.numeric, defensespending, nonhispwhite)
ols3 <- lm(y ~ ., data=dat3)
#
effect_plot(ols3, pred="immigrationlevel.numeric", interval=TRUE)
#
# Plotting with variable transformations
#
ols4 <- lm(y ~ partyid + log(defensespending), data=dat)
#
effect_plot(ols4, pred="defensespending", interval=TRUE)
effect_plot(ols4.dem, pred="defensespending", interval=TRUE, data=dat[democrat==1,], main="Democrats") + ylim(-100,100)
#
predict(ols4, dat[1:3,], interval="predict")
predict(ols4, dat[1:3,], interval="confidence")
#
#
# SUBSETTING
#
democrat <- NA
democrat[ANES2020$V201231x >= 4 & ANES2020$V201231x <= 7] <- 0
democrat[ANES2020$V201231x >= 1 & ANES2020$V201231x <= 3] <- 1
#
republican[ANES2020$V201231x >= 1 & ANES2020$V201231x <= 4] <- 0
republican[ANES2020$V201231x >= 5 & ANES2020$V201231x <= 7] <- 1
#
ols4.dem <- lm(y ~ log(defensespending), data=dat[democrat==1,])
ols4.rep <- lm(y ~ log(defensespending), data=dat[republican==1,])
#
effect_plot(ols4.dem, pred="defensespending", interval=TRUE, data=dat[democrat==1,], main="Democrats") + ylim=c(-100,100)
effect_plot(ols4.rep, pred="defensespending", interval=TRUE, data=dat[republican==1,], main="Republicans") + ylim=c(-100,100)
#
