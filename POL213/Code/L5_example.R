# load the AER package
library(AER)

# load the data set
data(CASchools)   

# define variables
CASchools$STR <- CASchools$students/CASchools$teachers       
CASchools$score <- (CASchools$read + CASchools$math)/2

# compute correlations
cor(CASchools$STR, CASchools$score)

cor(CASchools$STR, CASchools$english)

mod <- lm(score ~ STR, data = CASchools) 
mult.mod <- lm(score ~ STR + english, data = CASchools)

mod
mult.mod


# define the fraction of English learners        
CASchools$FracEL <- CASchools$english / 100

# estimate the model
mult.mod <- lm(score ~ STR + english + FracEL, data = CASchools) 

# obtain a summary of the model
summary(mult.mod)   


# set seed for reproducibility
set.seed(1)

# generate artificial data on location
CASchools$direction <- sample(c("West", "North", "South", "East"), 
                              420, 
                              replace = T)
summary(as.factor(CASchools$direction))

# estimate the model
mult.mod <- lm(score ~ STR + english + direction, data = CASchools)
mult.mod.intercept <- lm(score ~ STR + english + direction - 1, data = CASchools)

# obtain a model summary
summary(mult.mod) 
summary(mult.mod.intercept)


##### SIMULATION #######

# load packages
library(MASS)
library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  # for cov(X_1,X_2) = 0.25
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
  # for cov(X_1,X_2) = 0.85
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}


head(coefs1)


par(mfrow = c(2,2))
hist(coefs1[,1], xlim = c(0,6))
hist(coefs1[,2], xlim = c(0,6))
hist(coefs2[,1], xlim = c(0,6))
hist(coefs2[,2], xlim = c(0,6))

# obtain variance estimates
diag(var(coefs1)) #.25 some multicol

diag(var(coefs2)) #.85 severe multicol

