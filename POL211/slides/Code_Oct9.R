## 

library(tidyverse)

## Set wd

setwd("~/Dropbox/UCDavis/03_Teaching/02_211_Fall2023")

## Load data

df <- read_csv("Data/chetty_data.csv")

## what does this look like?

head(df)
glimpse(df)

## Let's look at some variables
## par_median: median income of parents
## sat_avg_2013: SAT avg. in 2013 among students 
## par_top1pc: % of parents in top 1% of income distribution

## We use sat_avg_2013, par_median
## Note that it's easier if we remove missings now
## So we don't always have to do na.rm = T

sat_avg_2013 <- na.omit(df$sat_avg_2013)
sat_avg_2013 <- df$sat_avg_2013[!is.na(df$sat_avg_2013)]

## Let's first subset to California
## Based on the variable "state"

df_ca <- df %>% 
  filter(state == "CA")
glimpse(df_ca)

## First, let's check the mean and median of par_median in CA

mean(df_ca$par_median)
median(df_ca$par_median)

## Histogram of par_median in CA

ggplot(df_ca, aes(x = par_median)) +
  geom_histogram()

## Variance 

v <- var(df_ca$par_median)

## SD

sqrt(v)
sd(df_ca$par_median)

## IQR

## Difference between the 75th percentile and the 25th percentile

### We can do this (i) directly and (ii) via "quantile"

IQR(df_ca$par_median)

## We can also calculcate the quartiles separately

q25 <- quantile(df_ca$par_median, probs = 0.25)
q75 <- quantile(df_ca$par_median, probs = 0.75)

## Subtract 
## Gives us the same result

q75 - q25

## Quantiles - deciles?

quantile(df_ca$par_median, probs = seq(0, 1, 0.1))
median(df_ca$par_median)

## Note that the median is equal to the 50th percentile / 5ht decile
