#
# POL 212 - Winter 2024
# probabilityMC.R
#
#
# Equation 1: f(X_{i}) = E(y | X_{i})
#
N <- 30
height <- rnorm(N, 66, 8)
hist(height)
expected.weight <- height * 2.2
plot(height,  expected.weight)
#
#
# Equation 2: y_{i} = f(X_{i}) + \epsilon_{i}
#
error.term <- rnorm(N, 0, 10)
weight <- expected.weight + error.term
plot(height,  weight)
#
#
# Estimate parameters with linear regression model
#
res <- lm(weight ~ height)
res$coefficients
#
plot(height, weight)
abline(a=res$coef[1], b=res$coef[2])
#
predict(res, newdata=data.frame(height=65))
#
plot(density(weight[height>65 & height<70]))
#
#
#
# The Monte Carlo principle: solutions through simulations
#  Example: what's the area of a circle?
#
library(plotrix)
library(grid)
#
plot(c(-1, 1), c(-1, 1), type = "n", asp=1)
rect( -1, -1, 1, 1)
draw.circle( 0, 0, 1)
nsamp <- 10000
inside <- NA
#
for(i in 1:nsamp){
  x <- runif(2, -1, 1)
  if (sqrt(x[1]*x[1] + x[2]*x[2]) < 1) {
    inside[i] <- 1; points(x[1],x[2], col="orange")}
  if(sqrt(x[1]*x[1] + x[2]*x[2]) > 1) {
    inside[i] <- 0; points(x[1], x[2], col="black")}
}
#
#
#
#
#
