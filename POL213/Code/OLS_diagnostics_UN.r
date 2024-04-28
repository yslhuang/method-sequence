#### POL 213 - Lecture 4 exercises ------------------------
## Author: Lauren Peritz
## Data: social indicators collected by UN in 1998
## Variables: tfr = total fertility rate in 1998 by country
## 

library(stargazer)
library(car)


UN <- read.table(
  "http://socserv.mcmaster.ca/jfox/Books/Applied-Regression-2E/datasets/UnitedNations.txt",
  header=TRUE)


head(UN)


# Bivariate and multivariate regressions
reg1 <- lm(tfr ~ GDPperCapita, UN)
reg2 <- lm(tfr ~ illiteracyFemale, UN)
reg3 <- lm(tfr ~ contraception, UN)
reg4 <- lm(tfr ~ GDPperCapita + illiteracyFemale + contraception, UN)
reg5 <- lm(tfr ~ GDPperCapita + illiteracyFemale + contraception + as.factor(region), UN)

stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", no.space = T)

# which countries influence the fit with respect to each observation?
avPlots(reg4, ask=FALSE)  # added-variable plots



# evaluating LEVERAGE using hat values

plot(hatvalues(reg4))  # index plot of hat-values
abline(h=c(3, 4)*mean(hatvalues(reg4)))  # three and four-times average hat-value
identify(hatvalues(reg4)) # identify high leverage points

order(hatvalues(reg4))



# we identified points 20  41  53  74  93 111

print(UN[c(20,  41,  53,  74,  93, 111),])

reg4_out <- lm(tfr ~ GDPperCapita + illiteracyFemale + contraception, UN[-c(20,  41,  53,  74,  93, 111)])



# evaluating OUTLIERS using studentized residual

outlierTest(reg4)  # Bonferroni t-test

qqPlot(reg4, simulate=TRUE, line="none")  # QQ plot for stud. res.
qqPlot(reg5, simulate=TRUE, line="none")  # QQ plot for stud. res.

# influence test
influencePlot(reg4)

# 4 plots: residual v. fitted, normal probability, scale location, residual v. leverage
plot(reg4)

