### PURPOSE: Solution key for Problem Set 1
### PREPARED FOR: POL213
### PREPARED BY: Lily Huang
### DATE: April, 20 2023

# Question 1

# load data
df <- read.csv("data/ps1_prestige.csv")

# calculate B
b <- sum((df$education-mean(df$education))*(df$prestige-mean(df$prestige)))/
    sum((df$education-mean(df$education))^2)

# calculate A
a <- mean(df$prestige)-b*mean(df$education)

# calculate residual standard error
yhat <- a + b*df$education
ei <- df$prestige - yhat
rse <- sqrt(sum(ei^2)/(10-2))

# calculate tss
tss <- sum((df$prestige - mean(df$prestige))^2)

# calculate rss
rss <- sum((df$prestige - yhat)^2)

# Question 2

# load data
df2 <- read.delim("data/Anscombe.txt", sep = "")

# a. scatterplots
library(car)
scatterplot(education ~ income, data = df2)
scatterplot(education ~ under18, data = df2)
scatterplot(education ~ urban, data = df2)

# b. simple linear regression
m1 <- lm(education ~ income, data = df2)
m2 <- lm(education ~ under18, data = df2)
m3 <- lm(education ~ urban, data = df2)

library(stargazer)
stargazer(m1, m2, m3, digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001))

# d. multiple linear regression
m4 <- lm(education ~ income + under18 + urban, data = df2)
stargazer(m1, m2, m3, m4, digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001))







