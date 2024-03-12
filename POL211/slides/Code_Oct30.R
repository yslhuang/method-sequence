#### Binomial distribution ####

N <- 200 # Number of trials
p <- 0.4 # p

## Generate N draws from a binomial distribution with parameter p
## Note that X is defined as the number of 'successes'

x <- rbinom(N, 1, p)

## Binomial distribution has 2 parameters, N, p
## Expected of a binomial RV is E(X) = N*p

## Check the observed number of successes

sum(x == 1)

#### Uniform RV ###

a <- 0
b <- 1

## 2 Parameters, a and b

## E(X) = (b+a)/2
## Var(X) = (a – b)2/12

## Calculate E(X) by hand

(b + a) / 2

## Calculate Var(X) by hand

(a - b)^2 / 12

## Generate N draws from a uniform distribution
x <- runif(N, 0, 1)

## Check the mean and variance of x

mean(x)
var(x)

#### Normal RV ####

## Normal RV has 2 parameters, mean and variance
## E(X) = mean
## Var(X) = variance

## Generate N draws from a normal distribution

x <- rnorm(N, mean = 0, sd = 1)

## We can use the random draws to approximate the CDF
## Share of obs smaller than 1.96?
## Approximation of P(X < 1.96), which is the CDF

sum(x < 1.96)/N

## We can also use the pnorm function to get the CDF directly

pnorm(1.96)

## This is the "exact" value
## It is just the CDP, i.e. P(X < x)
## We can also get the reverse : for a given P(X < x) what is x?
## Eg what is x such that P(X < x) = 0.5?

qnorm(0.5)

## This is the median of the normal distribution
## In the case of the normal distribution, the median is equal to the mean

## what is x such that P(X < x) = 0.95?
## Again, we can use qnorm to get the inverse of the CDF

qnorm(0.95)
