### PURPOSE: Solution key for Problem Set 2
### PREPARED FOR: POL213
### PREPARED BY: Lily Huang
### DATE: May, 2024

# load packages
library(tidyverse)
library(car)
library(stargazer)

# Question 1

# load data
house <- readRDS("/Users/yu-shiuanhuang/Desktop/method-sequence/data/AED_HOUSE2015.RDS") %>%
  mutate(region.f = factor(region, level = c("central", "east", "north", "south", "west")),
         price_1000 = price/1000)
  
# 1-a
mod <- lm(price_1000 ~ size + bedrooms + bathrooms + daysonmarket + region.f, data = house)
stargazer(mod, 
          title = "Table 1: Factors Explaining California Housing Price in 2015",
          dep.var.labels = "Housing Price",
          covariate.labels = c("Size", "Bedrooms", "Bathrooms", "Days on Market", 
                               "Region: East", "Region: North", "Region: South", 
                               "Region: West"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 2)

# 1-b
# create dummies for region 
house <- fastDummies::dummy_cols(house, select_columns = "region")

# convert each variable to a vector
price_1000 <- house$price_1000
intercept <- rep(1, nrow(house)) # create this vector with 1s!
size <- house$size
bed <- house$bedrooms
bath <- house$bathrooms
days <- house$daysonmarket
east <- house$region_east
north <- house$region_north
south <- house$region_south
west <- house$region_west

X <- cbind(intercept, size, bed, bath, days, east, north, south, west)

beta.hat <- solve(t(X) %*% X) %*% t(X) %*% price_1000
t(beta.hat)

# 1-c
# transform continuous variables to mean deviation
price_1000_m <- house$price_1000 - mean(house$price_1000 )
intercept <- rep(1, nrow(house)) # create this vector with 1s!
size_m <- house$size - mean(house$size)
bed_m <- house$bedrooms - mean(house$bedrooms)
bath_m <- house$bathrooms - mean(house$bathrooms)
days_m <- house$daysonmarket - mean(house$daysonmarket)
X_m <- cbind(intercept, size_m, bed_m, bath_m, days_m, east, north, south, west)

beta.hat.m <- solve(t(X_m) %*% X_m) %*% t(X_m) %*% price_1000_m
t(beta.hat.m)

# Question 2

# 2-b

# case 1: sample with spread out x
set.seed(123)

x <- runif(1000, min = -20, max = 20)
y <- 3 + 2*x + rnorm(1000, mean = 0, sd = 10)
df <- data.frame(x = x, y = y)

p1 <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "darkgray", alpha = 0.7) + 
  geom_abline(intercept = 3, slope = 2, color = "steelblue2", lwd = 1) +
  xlim(-20, 20) +
  labs(title = "x is widely spread") + 
  theme_bw()

# case 2: sample with clustered x
set.seed(123)
theta <- runif(500, min = 0, max = 2*pi) 
radius <- sqrt(runif(500, min = 0, max = 100))
x <- radius * cos(theta)
y <- radius * sin(theta)
df <- data.frame(x = x, y = y)

p2 <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "darkgray", alpha = 0.7) + 
  geom_abline(intercept = 3, slope = 3, color = "steelblue2", lwd = 1) +
  geom_abline(intercept = 3, slope = 1, color = "salmon2", lwd = 1) +
  geom_abline(intercept = 0, slope = -2, color = "darkseagreen", lwd = 1) +
  xlim(-20, 20) + ylim(-20, 20) +
  labs(title = "x is clustered") + 
  theme_bw()

ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1)


# Question 3

# 3-a
# generate 100 observations
eps <- rnorm(100, 0, 10)
X <- rep(1:50, 2)
D <- c(rep(0, 50), rep(1, 50))
Y <- 10 + X + D + (2*X*D) + eps

mod <- lm(Y ~ X)
mod2 <- lm(Y ~ X + D + X*D)
stargazer(mod, mod2, type = "text", digits = 2)

stargazer(mod, digits = 2)

ggplot() +
  geom_point(aes(x = mod$fitted.values, y = mod$residuals),
             color = "darkgray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkorange") +
  xlab("Fitted Values") + ylab("Residuals") +
  theme_bw()

plot(Y ~ X)

# Question 4

## load data
df <- read.csv("/Users/yu-shiuanhuang/Desktop/method-sequence/data/beauty.csv")

## regression
mod1 <- lm(eval ~ beauty + as.factor(female) + age + as.factor(minority) + 
             as.factor(nonenglish) + as.factor(lower), data = df)
plot(mod1)

library(stargazer)
stargazer(mod1, digits = 2)

## display the fitted model graphically
summary(df)
sd(df$eval)
beauty <- seq(-1.5, 2, 0.05) # beauty: -1.5~2
pred.df <- data.frame(beauty = beauty,
                      female = rep(0, length(beauty)),
                      age = rep(48.37, length(beauty)),
                      minority = rep(0, length(beauty)),
                      nonenglish = rep(0, length(beauty)),
                      lower = rep(0, length(beauty)))
plot.df <- data.frame(beauty = beauty,
                      predict(mod1, newdata = pred.df,
                              interval = "confidence", level = 0.95))
library(tidyverse)
library(ggrepel)
ggplot() +
    geom_point(data = df, aes(x = beauty, y = eval),
               alpha = 0.7, color = "darkgray") +
    geom_ribbon(data = plot.df, aes(x = beauty, ymin = lwr, ymax = upr),
                fill = "darkorange", alpha = 0.2) +
    geom_line(data = plot.df, aes(x = beauty, y = fit), color = "darkorange") +
    scale_x_continuous(name = "Composite Standardized Beauty Rating",
                       breaks = seq(-1.5, 2, 0.2)) +
    ylab("Course Evaluation") +
    theme_bw()

## assessing leverage
hat.df <- data.frame(case_id = as.numeric(names(hatvalues(mod1))),
                     hatvalues = hatvalues(mod1))
mean_hat <- (6+1)/nrow(df)

ggplot(hat.df, aes(x = case_id, y = hatvalues, label = case_id)) +
    geom_point(alpha = 0.7, color = "darkgray") +
    geom_hline(yintercept = 2*mean_hat, linetype = "dashed", color = "darkorange") +
    geom_hline(yintercept = 3*mean_hat, linetype = "dashed", color = "dodgerblue") +
    geom_text_repel(aes(label = ifelse(hatvalues >  2*mean_hat,
                                       as.character(case_id), "")),
                    hjust = -0.8, vjust = 0) +
    xlab("Case ID") + ylab("Hatvalues") +
    theme_bw()

## assessing discrepancy
df2 <- data.frame(fit = mod1$fitted.values,
                  residual = mod1$residuals,
                  rstudent = rstudent(mod1),
                  case_id = c(1:nrow(df)))
outlierTest(mod1)

p1 <- ggplot(df2, aes(x = fit, y = residual, label = case_id)) +
    geom_point(alpha = 0.7, color = "darkgray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkorange") +
    geom_text_repel(aes(label = ifelse(residual < -1 | residual > 1,
                                       as.character(case_id), "")),
              hjust = -0.8, vjust = 0) +
    scale_y_continuous(breaks = seq(-4, 2.5, 1), limits = c(-4, 2.5)) +
    xlab("Fitted Course Evaluation") +
    ylab("Residuals") +
    theme_bw()


p2 <- ggplot(df2, aes(x = fit, y = rstudent, label = case_id)) +
    geom_jitter(alpha = 0.5, color = "darkgray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkorange") +
    geom_text_repel(aes(label = ifelse(rstudent < -1.5 | rstudent > 1.5,
                                       as.character(case_id), "")),
              hjust = -0.8, vjust = 0) +
    scale_y_continuous(breaks = seq(-4, 2.5, 1), limits = c(-4, 2.5)) +
    xlab("Fitted Course Evaluation") +
    ylab("Studentized Residuals") +
    theme_bw()

ggpubr::ggarrange(p1, p2)

## assessing influence
library(car)
influencePlot(mod1)

## assess linearity assumption using C+R plots
crPlots(mod1)






