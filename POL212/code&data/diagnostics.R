#
# POL 212 - Winter 2024
# diagnostics.R
#
library(car)
library(jtools)
library(haven)
library(dplyr)
#
states <- data.frame(state.x77)
names(states) <- gsub(" ", ".", names(states))
#
ols1 <- lm(Life.Exp ~ Frost + Murder, data=states)
#
qqPlot(ols1)
#
plot(density(rstudent(ols1)))
#