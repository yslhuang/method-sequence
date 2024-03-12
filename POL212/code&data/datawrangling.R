#
# POL 212 - Winter 2024
# datawrangling.R
#
library(car)
library(jtools)
library(haven)
library(dplyr)
#
# Loops
#
Nsims <- 100
even.numbers <- matrix(pi, nrow=Nsims, ncol=2)
for (i in 1:Nsims){
  even.numbers[i,1] <- i
  even.numbers[i,2] <- i*2
  }
#
# Functions
#
beavis <- function(guess=7, bounds=0:10){
  actual <- sample(bounds, 1)
  compare <- actual - guess
  if(compare==0){print("Congratulations! Winner")}
  if(compare > 0){print("You lose, too low!")}
  if(compare < 0){print("You lose, too high!")}
  print(paste("Actual value was:", actual))
  return(list(whatiguessed=guess, actual=actual))
}
#
butthead <- beavis()
beavis(guess=3, bounds=c(3,3))
#
# Simulate a full OLS (y = bx + e)
#
set.seed(1234) # Set the seed for reproducible results
# Set true values of the parameters
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
# Create a sample of n observations on the independent variable X
X <- runif(n, -1, 1)
# Specify DGP (systematic + stochastic components)
Y <- b0 + b1*X + rnorm(n, 0, 1) # bX plus N(0, 1) error
# Estimate the OLS model
ols.model <- lm(Y ~ X) # Estimate OLS model
par.est <- NA # Initialize an object (par.est) to store parameter estimates
par.est[1] <- ols.model$coef[1] # Estimate for the intercept
par.est[2] <- ols.model$coef[2] # Estimate for the coefficient on X
#
#
# Transforming variables
#
scatterplot(prestige ~ income, data=Prestige)
scatterplot(prestige ~ log(income), data=Prestige)
#
# Nominal/ordinal variables
#
class(Prestige$type)
summary(lm(prestige ~ log(income) + type, data=Prestige))
jtools::summ(lm(prestige ~ log(income) + type, data=Prestige))
#
hist(Prestige$income)
income.binned1 <- cut(Prestige$income, breaks=3) # will be factor
income.binned2 <- cut(Prestige$income, breaks=3, labels=FALSE) # will be numeric
income.binned3 <- dplyr::ntile(Prestige$income, n=3) # will be numeric
data.frame(Prestige$income, income.binned1, income.binned2, income.binned3)
#
income.binned1
income.binned2
income.binned3
#
table(income.binned1, income.binned2)
table(income.binned2, income.binned3)
#
jtools::summ(lm(prestige ~ income.binned1 + type, data=Prestige))
jtools::summ(lm(prestige ~ income.binned2 + type, data=Prestige))
jtools::summ(lm(prestige ~ income.binned3 + type, data=Prestige))
#
income.temp <- income.binned2
ols.incomebinned2 <- lm(prestige ~ income.temp + type, data=Prestige)
income.temp <- income.binned3
ols.incomebinned3 <- lm(prestige ~ income.temp + type, data=Prestige)
#
jtools::plot_summs(ols.incomebinned2, ols.incomebinned3, model.names=c("cut()","ntile()"))
#
#
# Rescaling variables
#
Prestige$income
income.inthousands <- Prestige$income / 1000
summary(lm(prestige ~ income + type, data=Prestige))
summary(lm(prestige ~ income.inthousands + type, data=Prestige))
#
#
# Creating an index variable
#
Prestige$education
Prestige$income
cor(Prestige$education, Prestige$income)
#
education.stand <- scale(Prestige$education) # standardize education values
income.stand <- scale(Prestige$income) # standardize income values
SES <- (education.stand + income.stand) / 2
cor(data.frame(SES, Prestige$education, Prestige$income))
#
psych::alpha(data.frame(Prestige$income, Prestige$education))
#
