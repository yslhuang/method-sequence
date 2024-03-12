#
# POL 212 - Winter 2024
# interactions.R
#
library(car)
library(jtools)
library(haven)
library(dplyr)
library(interflex)  ## see https://yiqingxu.org/packages/interflex/RGuide.html and https://www.cambridge.org/core/journals/political-analysis/article/how-much-should-we-trust-estimates-from-multiplicative-interaction-models-simple-tools-to-improve-empirical-practice/D8CAACB473F9B1EE256F43B38E458706
library(interactions)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(performance)
library(see)
library(datawizard)
library(viridis)
library(statar)
library(rpart)
library(party)
library(rpart.plot)
#
# MODELING 2020 VOTE CHOICE
#
setwd("D:/")
ANES2020 <- read_dta("Dropbox/Files/Davis/Winter 2024/POL 212/data/ANES2020.dta")
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
#  Party identification
#  1 = STRONG DEMOCRAT, 7 = STRONG REPUBLICAN
#
partyid <- ANES2020$V201231x
partyid[partyid < 1] <- NA
partyid[partyid > 7] <- NA
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
# Race
#
nonhispwhite <- ANES2020$V201549x
nonhispwhite[ANES2020$V201549x==1] <- 1
nonhispwhite[ANES2020$V201549x!=1] <- 0
#
#
# ESTIMATE LINEAR REGRESSION MODEL
#
dat <- data.frame(y=candidate.difference, partyid, libcon, immigrationlevel, defensespending, nonhispwhite, correctplacement)
#
ols <- lm(y ~ ., data=dat)
summary(ols)
#
#
# INTERACTIONS
#
ols.interact <- lm(y ~ libcon:correctplacement +., data=dat)
summary(ols.interact)
#
interactions::interact_plot(ols.interact, pred=libcon, modx=correctplacement)
interactions::interact_plot(ols.interact, pred=correctplacement, modx=libcon)
#
partyid.bin1 <- statar::xtile(dat$partyid, 3)
partyid.bin2 <- ntile(dat$partyid, 3)
#
dat$partyid.bin1 <- factor(partyid.bin1)
ols.interact2 <- lm(y ~ libcon:partyid.bin1 +., data=dat)
#
# Continuous-by-continuous interaction
#
ols.interact2 <- lm(y ~ libcon:partyid +., data=dat)
summary(ols.interact2)
#
interactions::interact_plot(ols.interact2, pred=libcon, modx=partyid)
#
interactions::interact_plot(ols.interact2, pred=libcon, modx=partyid, modx.values=c(1:7))
#
# Let's try a more nonparametric approach
#
ggplot(na.omit(dat), aes(x=libcon, y=y, color=factor(partyid))) +
  geom_jitter(alpha=0.1) +
  stat_smooth(method = "lm") +
  scale_color_viridis(discrete=TRUE, option="turbo")
#
# Interflex package (Hainmueller et al. 2019, "How Much Should We Trust Estimates from Multiplicative Interaction Models?")
#
#
ols.interaction.binning <-interflex(estimator="binning", Y="y", D="libcon", X="partyid",
  Z=c("libcon","defensespending","nonhispwhite", "correctplacement"),
  data=dat, na.rm=TRUE)
plot(ols.interaction.binning)
#
ols.interaction.linear <-interflex(estimator="linear", Y="y", D="libcon", X="partyid",
  Z=c("libcon","defensespending","nonhispwhite", "correctplacement"),
  data=dat, na.rm=TRUE)
plot(ols.interaction.linear)
#
#
# FUN WITH MONTE CARLO and the INTERFLEX package
#
data(interflex)
ls()
#
# <We won't use these, but here they are anyway:>
#  s1 is a case of a dichotomous treatment indicator with linear marginal effects;
#  s2 is a case of a continuous treatment indicator with linear marginal effects;
#  s3 is a case of a dichotomous treatment indicator with nonlinear marginal effects;
#  s4 is a case of a dichotomous treatment indicator, nonlinear marginal effects, with additive two-way fixed effects;
#  s5 is a case of a discrete treatment indicator, nonlinear marginal effects, with additive two-way fixed effects.
#
set.seed(1234)
moderator <- rnorm(1000, 0, 1)
set.seed(1234)
covariate <- runif(1000, -1, 1)
set.seed(2345)
D <- rbinom(1000, 0:1, 0.5)
treatment <- NA
treatment[D==0] <- "control"
treatment[D==1] <- "treated"
set.seed(3456)
outcome_green <- 0.3*moderator + 0.4*D + 0.6*moderator*D + 0.2*covariate + rnorm(1000, 0, 0.2)
outcome_red <- 0.3*moderator + 0.4*D + D*(moderator^2-2.5) + 0.2*covariate + rnorm(1000, 0, 0.2)
#
beavis <- data.frame(moderator, covariate, treatment, outcome_green, outcome_red)
#
# "Green light" outcome
interflex(estimator = "raw", Y = "outcome_green", D = "treatment", X = "moderator", Z = "covariate",
  data = beavis, weights = NULL, Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator",
  main = "Raw Plot", cex.main = 1.2, ncols=2)
butthead <- interflex(Y = "outcome_green", D = "treatment", X = "moderator", Z = "covariate",
  data = beavis, estimator = "binning", main = "Marginal Effects", ylim = c(-15, 15))
plot(butthead)
#
# "Red light" outcome
interflex(estimator = "raw", Y = "outcome_red", D = "treatment", X = "moderator", Z = "covariate",
  data = beavis, weights = NULL, Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator",
  main = "Raw Plot", cex.main = 1.2, ncols=2)
butthead <- interflex(Y = "outcome_red", D = "treatment", X = "moderator", Z = "covariate",
  data = beavis, estimator = "binning", main = "Marginal Effects", ylim = c(-15, 15))
plot(butthead)
#
#
#
# TREES
#
# Load data
#
swiss.data <- read_dta("Dropbox/Files/Davis/Winter 2024/POL 212/data/swiss.dta")
dat <- swiss.data
#
# Fit classification tree
#
set.seed(2023)
fit <- rpart(Fertility ~ ., data = dat,
  control = rpart.control(minsplit = 20, minbucket = 5))
#
# Plot tree
#
plot(fit, uniform=TRUE, main="Classification Tree for the 2016 ANES Data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
#
# Nicer plot
#
rpart.plot(fit)
#
y <- dat$Fertility
yhat <- predict(fit, dat)
#
residuals <- (y - yhat)
sq.residuals <- residuals^2
SSR <- sum(sq.residuals)
#
cor(y, yhat)^2
#
lm.res <- lm(Fertility ~ . + Education*Catholic, data = dat)
lm.y <- dat$Fertility
lm.yhat <- predict(lm.res, dat)
#
lm.residuals <- (lm.y - lm.yhat)
lm.sq.residuals <- lm.residuals^2
lm.SSR <- sum(lm.sq.residuals)
#
cor(lm.y, lm.yhat)^2
#
#
#
# Another example of a regression tree
#
set.seed(1234)
X <- matrix(runif(5000,0,1), nrow=500, ncol=10)
set.seed(1985)
y <- X[,1] + 2 * X[,2] + 2*(X[,3] * X[,4]) + rnorm(500, 0, 1)
regdat <- data.frame(y, X)
#
reg.tree <- rpart(y ~ ., data = regdat,
                  control = rpart.control(minsplit = 20, minbucket = 10))
#
rpart.plot(reg.tree)
#
reg.tree <- rpart(y ~ ., data = regdat[sample(1:nrow(regdat), 200, replace=TRUE),],
             control = rpart.control(minsplit = 20, minbucket = 10))
#
rpart.plot(reg.tree)
#
#
