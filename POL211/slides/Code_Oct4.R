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

## Let's first subset to California
## Based on the variable "state"

df_ca <- df %>% 
  filter(state == "CA")
glimpse(df_ca)

## What is the mean of median parental income in 2013

df %>% summarise(mean_parental = mean(par_median))
summary(df$par_median)
summary(df[, "par_median"])
summary(df[, 7])

## What is the mean of parents in top 1%?

mean(df$par_top1pc)

## What is the mean SAT score?
## Note that this has missings, so we use na.rm = T

mean(df$sat_avg_2013, na.rm = T)

## What are the colleges that score highest on these variables?

df_ca <- df_ca %>% 
  arrange(desc(par_top1pc))

head(df_ca %>% select(name, par_top1pc))

## Same for median parental income

df_ca <- df_ca %>% 
  arrange(desc(par_median))

head(df_ca %>% select(name, par_top1pc))

## Same for SAT scores 


df_ca <- df_ca %>% 
  arrange(desc(sat_avg_2013))

## Now, can we calculate the same quantities separately for public / private schools?

df_ca_summary <- df_ca %>% 
  group_by(public) %>% 
  summarise(mean(par_top1pc))
