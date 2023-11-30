library(tidyverse)

#### Example 1 ####

## Amtrak is very proud of the capitol corridor train 
## from the Bay Area to Davis
## they argue that 90% of the time, it is less than 5 min late

## You ride the train 100 times, but you find that it is late 15 
## out of 100 times

## What distribution can we use here?

## Binomial distribution with Bin(N = 100, p = 0.1)

## What is the null hypothesis about the number of times the 
## train is late for 100 rides?

## We assume a binomial distribution
## H_0: p = 0.1
## H_a: p > 0.1

## How can we test this hypothesis?
## We are asking: given the null hypothesis, how likely is it 
## that we would see 15 or more late trains out of 100 rides?

library(tidyverse)

x <- seq(0, 30)
y <- dbinom(x, 100, 0.1)
ggplot(data= data.frame(x, y), 
aes(x, y)) +
    geom_bar(stat = "identity") +
      geom_vline(xintercept = 15, color = "black") +
      xlab("Number of times train is late") +
      ylab("Probability") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 30, 5)) +
      ggtitle("Probability of seeing x late trains out of 100 rides")

## What is the probability of seeing 15 or more late trains 
## out of 100 rides?

1 - pbinom(14, 100, 0.1)

## In mathematical notation: P(X >= 15 | H_0)
## This is equal to 1-P(X < 15 | H_0) = 1-F(14), 
# where F is the CDF of the binomial distribution under the null

#### Example 2 ####

# A friend argues that the average professor at UC Davis spends 
# 100 dollars a month on coffee

# You sample 100 professors and find that they spend 150 dollar 
# a month on coffee, on average

# X_bar = 150
# SE(X_bar) = 25

# You also calculate the standard error of the mean, 
# which is 25 dollars

## What is the null hypothesis?
## Statement about the population mean mu

# mu = 100

## What is the alternative hypothesis?

# mu != 100 (version 1, two-tailed test)
# mu > 100 (version 2, one-tailed test)

## What is the distribution of the sample mean under the null? 

# Normally distributed with X_bar ~ N(100, 25)

## Distribution under the null (plot)

library(tidyverse)

x <- seq(0, 200, 0.1)
f_x <- dnorm(x, 100, 25)

p2 <- ggplot(data = data.frame(x, f_x), aes(x, f_x)) +
  geom_line() +
  geom_vline(xintercept = 150, color = "black") +
  geom_vline(xintercept = 100, color = "black", linetype = "dotted") +
  xlab("Average monthly coffee spending") +
  ylab("Density") +
  theme_minimal() +
  annotate("text", x = 155, y = 0.01, 
           label = "Observed sample mean", angle = 90) +
  annotate("text", x = 105, y = 0.01, 
           label = "True value under the null", angle = 90) +
  ggtitle("Approx. distribution of the sample mean under the null")
p2

## Area under the curve to the right of 150
## This is the one-tailed p-value

p2_onetailed <- p2 + 
  geom_area(data = data.frame(x = seq(150, 200, 0.1), 
                              f_x = dnorm(seq(150, 200, 0.1), 
                                          100, 25)), 
            aes(x, f_x), fill = "red", alpha = 0.5)
p2_onetailed

## Two-tailed test: what is the probability of seeing an average 
## of 150 or more OR 50 or less?

p2_twotailed <- p2_onetailed +
  geom_area(
    data = data.frame(x = seq(0, 50, 0.1), 
                      f_x = dnorm(seq(0, 50, 0.1), 100, 25)),
    aes(x, f_x), fill = "red", alpha = 0.5
  )
p2_twotailed

## Basically asking: what is the probability of seeing a 
## "more extreme" value than 150?
## 

lower_p <- pnorm(50, mean =  100, sd = 25)
upper_p <- 1 - pnorm(150, mean = 100, sd = 25)

## Two tailed p-value

lower_p + upper_p

## One tailed p-value

upper_p

## We can instead calculate the t-stat
## This is the observed sample mean minus the true value 
## under the null divided by the standard error of the mean

t_stat <- (150 - 100) / 25

## Now, this has a distribution N(0,1) if the sample is large enough

## Plot this
## This plot shows a normal distribution 
## with mean 0 and SD 1

x <- seq(-3, 3, 0.01)
f_x <- dnorm(x, 0, 1)

p3 <- ggplot(data = data.frame(x, f_x), aes(x, f_x)) +
  geom_line() +
  geom_vline(xintercept = t_stat, color = "black", 
             linetype = "dotted") +
  geom_vline(xintercept = -t_stat, color = "black", 
             linetype = "dotted") +
  xlab("t-statistic") +
  ylab("Density") +
  theme_minimal() +
  annotate("text", x = t_stat + 0.1, y = 0.1, 
           label = "Observed t-statistic", angle = 90) +
  annotate("text", x = -t_stat - 0.1, y = 0.1, 
           label = "-Observed t-statistic", angle = 90) +
  ggtitle("Approx. distribution of the t-statistic under the null")
p3

## We can calculate the p-value using the CDF of the normal distribution

p_val <- (1 - pnorm(t_stat, 0, 1)) + pnorm(-t_stat, 0, 1)
p_val

#### Example 3 - regression ####

## We use the Chetty data again

chetty <- read_csv("~/Dropbox/UCDavis//03_Teaching/02_211_Fall2023/Data/chetty_data.csv") %>% 
  mutate(sticker_price_2013 = sticker_price_2013 / 1000)

glimpse(chetty)

## Is there a relationship between sticker price and above-average mobility rate 

## What is the null hypothesis?

## beta = 0

## What is the alternative hypothesis?

## beta != 0

## What is the test statistic

## Test statistic: estimated regression coefficient: beta_hat

## What is the distribution under the null

## Normal w/ beta_hat ~ N(0, SE(beta_hat))

## Let's estimate this 

m1 <- lm(mobility_above_average ~ sticker_price_2013, 
         data = chetty)

## Let's get the coef and the standard error

m1 <- summary(m1)
m1

## 

beta_hat <- m1$coefficients[2, 1]
se_beta_hat <- m1$coefficients[2, 2]

## Now under the null hypothesis, what is the probability of 
## seeing a coefficient of this size or more extreme?

## We can use the CDF of the distribution of the coefficient 
## under the null (under the null, the mean of estimate of beta is 0)

p_val <- pnorm(beta_hat, 0, se_beta_hat) +
  (1 - pnorm(-beta_hat, 0, se_beta_hat))
p_val

## We can also first calculate a t-statistic and then use the 
## normal distribution
## Note that here, we rely on the fact that for large samples, 
## the t-distribution is very similar to the normal distribution
## Since the t-stat follows a t-distribution

t_stat <- (beta_hat - 0) / se_beta_hat

upper <- 1-pnorm(-t_stat, 0, 1)
lower <- pnorm(t_stat, 0, 1)

## Note that the t-stat is negative, so we have to calculate 
## (i) the area of the PDF for values smaller than the t-stat and 
## (ii) the area of the PDF for values larger than t-stat multiplied by -1

p_val <- upper + lower
p_val

#### Show this graphically ####

x <- seq(-4, 4, 0.01)
y <- dnorm(x, 0, 1)

p1 <- ggplot(data = data.frame(x, y), aes(x, y)) +
  geom_line() +
  geom_vline(xintercept = t_stat, color = "black", 
             linetype = "dotted") +
  geom_vline(xintercept = -t_stat, color = "black", 
             linetype = "dotted") +
  xlab("t-statistic") +
  ylab("Density") +
  theme_minimal() +
  annotate("text", x = t_stat + 0.1, y = 0.1, 
           label = "Observed t-statistic", angle = 90) +
  annotate("text", x = -t_stat - 0.1, y = 0.1, 
           label = "-Observed t-statistic", angle = 90) +
  ggtitle("Approx. distribution of the t-statistic under the null")
p1
