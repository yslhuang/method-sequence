# POL 212 Midterm Solution

# Question 1
library(tidyverse)
library(haven)

## load data (2 points)
df <- read_dta("/Users/yu-shiuanhuang/Desktop/method-sequence/POL212/midterm/humanrights.dta")

## print the first six rows (1 points)
head(df)

# Question 2

## pearson correlation (2 points)
cor(df$time, df$humanrights) # 0.237365

## scatterplot with lowess (3 points)
ggplot(data = df, aes(x = time, y = humanrights)) +
  geom_point(alpha = 0.7, color = "darkgray") +
  geom_smooth(method = "loess", color = "coral2", se = FALSE) +
  ylab("Human Rights Index") + xlab("Time") +
  theme_bw()

# Question 3

## histogram of kingsfans (1 points)
ggplot(data = df, aes(x = kingsfans)) + 
  geom_histogram(bins = 30, color = "black", fill = "gray85") +
  xlab("Proportion of the population who are fans of the Sacramento Kings") +
  theme_bw()

## scatterplot with lowess (1 points)
ggplot(data = df, aes(x = kingsfans, y = humanrights)) +
  geom_point(alpha = 0.7, color = "darkgray") +
  geom_smooth(method = "loess", color = "coral2", se = FALSE) +
  ylab("Human Rights Index") + 
  xlab("Proportion of the population who are fans of the Sacramento Kings") +
  theme_bw()

## appropriate transformation (1 points)
ggplot(data = df, aes(x = log(kingsfans))) + 
  geom_histogram(bins = 30, color = "black", fill = "gray85") +
  xlab("Proportion of the population who are fans of the Sacramento Kings") +
  theme_bw()

ggplot(data = df, aes(x = log(kingsfans), y = humanrights)) +
  geom_point(alpha = 0.7, color = "darkgray") +
  geom_smooth(method = "loess", color = "coral2", se = FALSE) +
  ylab("Humanrights Index") + 
  xlab("Proportion of the population who are fans of the Sacramento Kings") +
  theme_bw()

# Question 4

## level of measurement (1 points)

## justify your choice (1 points)

## how do you code in R (1 points)
df$tort_c <- factor(df$tort, levels = c(2, 1, 0), 
                    labels = c("None", "Occasional", "Frequent"))

# Question 5

## 5a. run simple linear regression (1 points)
mod1 <- lm(humanrights ~ tort_c, data = df)
stargazer::stargazer(mod1, type = "text")

## 5a. report and interpret regression table (2 points)

## 5b. standardize humanrights (1 points)
df$humanrights_sd <- scale(df$humanrights)
summary(df$humanrights_sd)

## 5b. report and interpret regression table after standardization (2 points)
mod2 <- lm(humanrights_sd ~ tort_c, data = df)
stargazer::stargazer(mod2, type = "text")

# Question 6

## run multiple linear regression (0.5 points)
mod3 <- lm(humanrights ~ tort_c + genocide + time, data = df)
stargazer::stargazer(mod3, type = "text")


## 6a (2.5 points)
## all coefficients are significant

## 6b (3) points)
## The sign on time is positive, which indicates that our model will predict a higher human rights score
## for any given country-year if the time is further from 1980.

## 6c (3 points)
## R^2 = 0.638
## The closer the Rˆ2 is to 1, the more successful our model is at
## making predictions. Because our Rˆ2 is 0.6337, it means the included covariates “explain” about 63% of the
## variance in the dependent variable.

## 6d (3 points)
set.seed(1234)
r2 <- c()

for (i in 1:500){
  samp <- sample(1:nrow(df), 1000)
  mod <- lm(humanrights ~ tort_c + genocide + time, data = df[samp,])
  r2 <- c(r2, summary(mod)$r.squared)
}

ggplot(data = data.frame(r2), aes(x = r2)) + 
  geom_histogram(bins = 30, color = "black", fill = "gray85") +
  xlab("R2") +
  theme_bw()

mean(r2)
sd(r2)



