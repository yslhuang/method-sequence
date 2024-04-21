#
# OLS_optimization.R
#
library(scatterplot3d)  # basic plotting
library(plot3D)         # fancy plotting


# set seed
set.seed(1)

#
# Define function to calculate residual sum of squares for values of x, 
#  y, alpha, and beta
#
RSS <- function(x, y, alpha, beta){
  residuals <- y - (alpha + beta*x)
  return(sum(residuals^2))
}
#
#
# Simulate data (alpha=2, beta=3)
#
x.sim <- runif(100, -1, 1)
e.sim <- rnorm(100, 0, 1.5)
y.sim <- 2 + 3*x.sim + e.sim
#
#
# Run canned OLS - this is our goal
#
ols <- lm(y.sim ~ x.sim)
summary(ols)
#
sum(ols$residuals^2)
#
# Use RSS function to manually calculate score for individual parameter 
#  combinations
#
RSS(x=x.sim, y=y.sim, alpha=0, beta=0)
RSS(x=x.sim, y=y.sim, alpha=0, beta=1)
#
#
# Use RSS function to manually calculate score for range of parameter 
#  combinations
#
rss.eval <- NA
a.vals <- seq(-4, 6, by=0.1)
b.vals <- seq(-4, 6, by=0.1)
c.vals <- expand.grid(a.vals, b.vals)
for (i in 1:nrow(c.vals)){
  rss.eval[i] <- RSS(x=x.sim, y=y.sim, alpha=c.vals[i,1], beta=c.vals[i,2])
}
#

## Plot RSS as a function of alpha and beta

# simple
scatterplot3d(x=c.vals[,1], y=c.vals[,2], z=rss.eval,pch = ".",
              xlab="alpha", ylab="beta", zlab="RSS")

# fancy 
scatter3D(x=c.vals[,1], y=c.vals[,2], z=rss.eval, theta=30, phi=0, bty="g",
  xlab="alpha", ylab="beta", zlab="RSS", ticktype="detailed")
# different perspective
scatter3D(x=c.vals[,1], y=c.vals[,2], z=rss.eval, theta=-30, phi=40, bty="g", type="h",
  xlab="alpha", ylab="beta", zlab="RSS")

# what is the BLUE?
bob <- cbind(c.vals, rss.eval)
colnames(bob) <- c("alpha","beta","rss")
bob[bob$rss == min(bob[,3]),]

# compare
ols$coefficients

#
#
# Alternately, solve via optimization
#
RSS2 <- function(beta) {   ## combine unknown parameters in single vector (beta[1] = intercept; beta[2] = coefficient)
    residuals <- y.sim - (beta[1] + beta[2]*x.sim)
    return(sum(residuals^2))}
#
ols.optim <- optim(c(0,0), RSS2, method="BFGS", control=list(maxit=100000, reltol=0.0000001))
#
print(ols.optim)
#
sum(ols$residuals^2)
ols.optim$value
#
sqrt(ols.optim$value / 98)
#
