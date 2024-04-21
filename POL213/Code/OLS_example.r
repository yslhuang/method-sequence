## POL 213 - Regression with Several Explanatory Variables
## Weeks 2 & 3 - practice with data

library(carData)
View(Prestige)
d <- Prestige

## Multiple Linear Regression and Model Fit -------------------------------
## regression of prestige on explanatory variables

mod <- lm(prestige ~ education + income + women, data = d)
summary(mod)

## distributions of variables
ggplot(data = d, aes(x = prestige)) +
    geom_histogram(bins = 15, color = 'turquoise4')

ggplot(data = d, aes(x = education)) +
  geom_histogram(bins = 15, color = 'turquoise4')

ggplot(data = d, aes(x = income)) +
  geom_histogram(bins = 15, color = 'turquoise4')

ggplot(data = d, aes(x = women)) +
  geom_histogram(bins = 15, color = 'turquoise4')


## Evaluate Model Fit by calculating the R-Squared statistic

RSS <- sum((mod$residuals)^2)
SE <- sqrt(RSS/(nrow(d)-4))
TSS <- sum((d$prestige-mean(d$prestige))^2)

y_hat_byhand <- mod$coefficients[1] + mod$coefficients[2]*d$education + mod$coefficients[3]*d$income + mod$coefficients[4]*d$women
plot(y_hat_byhand, fitted.values(mod), pch = "*") # check matches

RegSS = sum((y_hat_byhand - mean(d$prestige))^2)

Rsqr = RegSS/TSS

Rsqr

summary(mod)

## Adjusted R^2 to penalize for too many variables

AdjRsqr <- 1 - (RSS / (nrow(d)-4))/ (TSS / (nrow(d)-1))
AdjRsqr


## Confidence Intervals and Hypothesis Tests -------------------------


# Let's simplify to just the two variables (education and income)
mod2 <- lm(prestige ~ education + income , d)
summary(mod2)

# this is our goal!

# Standard error of individual slope coefficients

RSS_mod2 <- sum((mod2$residuals)^2)
RegSS_mod2 = sum((fitted.values(mod2) - mean(d$prestige))^2)


# Standard error of B_0
mod_partial <- lm(education ~ income, d)
summary(mod_partial)$r.squared

mod_partial <- lm(income ~ education, d)
summary(mod_partial)$r.squared


# either way, define the r_j^2 as the R squared for fitting a model of X_j on all other Xs
r_12 <- summary(mod_partial)$r.squared

SE_mod2 = sqrt(RSS_mod2 / (nrow(d)-3))

SE_B1 <- 1/(sqrt(1-r_12)) * (SE_mod2 / sqrt(sum((d$education - mean(d$education))^2)))
SE_B2 <- 1/(sqrt(1-r_12)) * (SE_mod2 / sqrt(sum((d$income - mean(d$income))^2)))


summary(mod2)
SE_B1
SE_B2

# Checks out!

# Let's construct 95% confidence intervals for the slope coefficients.

# find two-tailed t critical value with 99 degrees of freedom
tcrit <- qt(p=0.05/2, df=99, lower.tail = TRUE)

B1_low <- mod2$coefficients[2] - tcrit*SE_B1
B1_hi <- mod2$coefficients[2] + tcrit*SE_B1

B2_low <- mod2$coefficients[3] - tcrit*SE_B2
B2_hi <- mod2$coefficients[3] + tcrit*SE_B2

CIs <- rbind(cbind(B1_low, B1_hi),cbind(B2_low, B2_hi))
colnames(CIs) <- c("low", "hi")
round(CIs, 8)

# added variable plot
car::avPlots(mod2)

## Are the sampling covariances of the least squares coefficients nonzero?
X <- cbind(rep(1, nrow(d)), d$education, d$income)
Y <- d$prestige
beta.hat <- solve(t(X)%*%X) %*% t(X) %*%Y
e <- Y - X%*%beta.hat

S2E <- as.numeric( (t(e)%*%e)/(nrow(d)-3) )

v.beta.hat <- S2E*solve(t(X)%*%X)
v.beta.hat


## F-test for "Omnibus" null hypothesis

F_0 <- (RegSS_mod2 / 2) / (RSS_mod2 / (nrow(d)-3))
F_0

# Compare to f statistic, checks out



### Compare nested models
mod <- lm(prestige ~ education + income + women, data = d) # complex model
mod2 <- lm(prestige ~ education + income, data = d)        # simple model

anova(mod2, mod, test = "F")

F_0nest <- ((RSS_mod2 - RSS)/1) / (RSS / (nrow(d)-3-1))

round(F_0nest,4)

# now look at the anova once again. Did the more complex model provide an improvement?

anova(mod2, mod, test = "F")

# No, we fail to reject the null (simpler) model. 
# Adding the "women" covariate did not significantly improve model fit. 