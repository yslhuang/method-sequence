library(tidyverse)

#### Confidence intervals ####

## Assume we want to estimate the amount of 
## money spent on coffee among all 
## UC Davis graduate students

## We take a random sample of 100 
## graduate students and ask them how much they 
## spend on coffee per week (in dollars)

## Assume know the population distribution: N(20, 8)

## Let's take a sample of 100 
## students and compute the sample mean

set.seed(1234)

sample <- rnorm(100, 20, 8)

mean(sample)

## ~ 18.74

## Ok; now we want to obtain more 
## information about plausible values for 
## the population mean
## We can do this by computing a confidence interval

## From the CLT, we know that the distribution 
## of the sample mean is approximately normal

## Usually we construct a symmetric confidence 
## interval around the sample mean

## [sample mean - c*SE(sample mean), 
## sample mean + c*SE(sample mean)]

## Our goal is to - with 95% probability - 
## construct a confidence interval that 
## contains the population mean
## Therefore, we have to choose c such that 
## 95% of the probability mass of the 
## normal distribution is contained in the interval

## Recall the shape of the normal distribution

x <- seq(-3, 3, 0.01)
f_x <- dnorm(x, 0, 1)
plot(x, f_x, type = "l")

## Say we want to construct a 95% confidence interval
## We need to find value of c such that 
## 95% of the probability mass is contained in 
## the interval [\bar{X}-c*SE(\bar{X}), \bar{X}+c*SE(\bar{X})]
## Ie we need to have 2.5% of the 
## probability mass in each tail
## What is the value of c?

qnorm(0.975)
qnorm(0.025)

## As before: ~1.96

## Let's construct the confidence interval
## First we need the SE of the sample mean

se_sample_mean <- sd(sample) / sqrt(100)
sample_mean <- mean(sample)

(ci_lower_bound <- sample_mean - 1.96 * se_sample_mean)
(ci_upper_bound <- sample_mean + 1.96 * se_sample_mean)

## show this graphically

plot(seq(15, 22, 0.01),
    dnorm(
        seq(15, 22, 0.01),
        sample_mean,
        se_sample_mean
    ),
    type = "l",
    xlab = "Coffee spending (sample mean)",
    ylab = "Density", 
    main = "Distribution of the sample mean"
)

abline(v = ci_lower_bound, col = "red")
abline(v = ci_upper_bound, col = "red")

## Correct interpretation of the CI
## If we repeat the experiment many times, 
## 95% of the time the population mean 
## will be in the CI
## Ie this assumes we compute the CI 
## each time we repeat the experiment

## We can simulate this 
## Define a function that computes the CI

get_ci <- function(x) {

    ## Compute SE of the sample mean, and the sample mean

    se_sample_mean <- sd(x) / sqrt(100)
    sample_mean <- mean(x)

    ## Compute CI
    
    ci_lb <- sample_mean - 1.96 * se_sample_mean
    ci_ub <- sample_mean + 1.96 * se_sample_mean
    
    data.frame(
        ci_lower_bound = ci_lb,
        ci_upper_bound = ci_ub
    )

}

## Do this across 100 samples
## Using the same distribution as above

set.seed(1)

n_samples <- 100

ci <- replicate(n_samples,
    get_ci(rnorm(100, 20, 8)),
    simplify = FALSE
)


## This is a list of DFs; need to transform to DF

ci_df <- ci %>%
    reduce(rbind) %>%
    mutate(ci_id = 1:n()) %>%
    mutate(ci_id = fct_reorder(
        as.character(ci_id),
        ci_lower_bound
    )) %>%
    mutate(mu_in_ci = (ci_lower_bound < 20) &
        (ci_upper_bound > 20))

## Plot this

ggplot(ci_df, aes(x = ci_id, group = mu_in_ci)) +
    geom_errorbar(aes(
        ymin = ci_lower_bound,
        ymax = ci_upper_bound, 
        color = mu_in_ci
    ), width = 0) +
    geom_hline(yintercept = 20, col = "red") +
        theme_bw() +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank()
        ) +
    scale_color_manual(
        values = c("red", "black"),
        labels = rev(c(
            "CI contains mu",
            "CI does not contain mu"
        )), name = ""
    ) +
        theme(legend.position = "bottom") +
            theme(
                panel.grid.major =
                    element_blank(), panel.grid.minor =
                    element_blank()
            ) +
            ylab("Confidence interval")

## Count the number of times that the
## true parameter is in the CI

sum((ci_df$ci_lower_bound < 20) &
    (ci_df$ci_upper_bound > 20))

## Across 5% of samples, the CI contains 
## the true parameters

#### 2. Connection to hypothesis testing: ####
## If the CI does not contain the null 
## hypothesis value, we reject the null hypothesis

## To see this, consider:
## we reject the null if the sample mean 
## (our test statistic) 
## is in the rejection region
## The rejection region is any value above:
##  mu_0 + 1.96 * SE(sample mean)
## Or any value below 
##  mu_0 - 1.96 * SE(sample mean)
## Here, mu_0 is the null hypothesis value
## Assume a null hypothesis of mu_0 = 15

mu_0 <- 15

## Rejection region: anything 
## outside this interval

c(mu_0 - 1.96 * se_sample_mean, 
  mu_0 + 1.96 * se_sample_mean) %>%
    round(2)

## CI of the sample mean

c(
    sample_mean - 1.96 * se_sample_mean,
    sample_mean + 1.96 * se_sample_mean
) %>% round(2)

## Let's plot the distribution under this null:

plot(seq(12, 24, 0.01),
    dnorm(
        seq(12, 24, 0.01), mu_0,
        se_sample_mean
    ),
    type = "l",
    xlab = "Coffee spending",
    ylab = "Density under the null",
    main = "Distribution of the sample mean under the null,\nand based on the data"
)

## Rejection region

abline(
    v = mu_0 - 1.96 * se_sample_mean,
    col = "red"
)
abline(
    v = mu_0 + 1.96 * se_sample_mean,
    col = "red"
)

## Distribution of the sample mean we use for the CI
## Blue lines mark the CI

lines(x = seq(15, 24, 0.01), y = dnorm(
    seq(15, 24, 0.01),
    sample_mean, se_sample_mean
), lty = 2)

abline(
    v = sample_mean - 1.96 * se_sample_mean,
    col = "blue"
)
abline(
    v = sample_mean + 1.96 * se_sample_mean,
    col = "blue"
)

## Add legend for the lines 

legend(
    "topright",
    legend = c(
        "Distribution under null",
        "Distribution based on data"
    ),
    lty = c(1, 2),
    col = c("black", "black"),
    bty = "nn"
)

## 

legend(
    "topleft",
    legend = c(
        "Rejection region",
        "Confidence interval"
    ),
    lty = c(1, 1),
    col = c("red", "blue"),
    bty = "nn"
)


#### Confidence intervals for linear regression coefficients ####

## Assume we stdy the relationship between
## Money spend on coffee and hours of sleep every night

set.seed(123)

## Here we use the sample from above

coffee_spending <- sample 

Y <- 8 - 0.1 * coffee_spending + rnorm(100, 0, 3)

## What does the slope tell us?

## Let's plot this

ggplot(data.frame(coffee_spending, Y), 
       aes(x = coffee_spending, y = Y)) +
    geom_point(shape = 21, fill = "white") +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        xlab("Coffee spending in USD") +
        ylab("Hours of sleep")
    

## Estimate the regression:

s <- summary(lm(Y ~ coffee_spending))
s

## Get the coef and SE 

beta_hat <- s$coefficients[2, 1]
se_beta_hat <- s$coefficients[2, 2]

## Construct a 95% CI

ci_lower_bound <- beta_hat - 1.96 * se_beta_hat
ci_upper_bound <- beta_hat + 1.96 * se_beta_hat

c(ci_lower_bound, ci_upper_bound)

## Does this include 0?

## No - we can reject the null

## Alternatively, we can also 
## conduct a standard hypothesis test
## H_0 : beta = 0
## H_A : beta != 0

## Here, we convert the
## test statistic (regression coefficient) 
## to a t-statistic

t_stat <- (beta_hat - 0) / se_beta_hat

## We can conduct the hypothesis 
## test using the t-statistic
## This is how R does it 

pt(t_stat, df = 98, lower.tail = TRUE) * 2

## This is equal to F(t_stat, 98) + (1-F(-t_stat, 98))

## Alternatively, we can use the p-value 
## using the normal distribution

2 * pnorm(t_stat)

## Equal to 2*F(t_stat) where F is the CDF 
## of the normal distribution

## Both are very similar
## In both cases, we reject the null hypothesis

## Bonus : this is often how the CI and 
## point estimate are plotted (coefficient plot)

coef_df <- data.frame(estimate = s$coefficients[, 1],
    se = s$coefficients[, 2],
    var = rownames(s$coefficients)
) %>%
    mutate(var = fct_reorder(var, estimate)) %>%
    mutate(
        ci_lower_bound = estimate - 1.96 * se,
        ci_upper_bound = estimate + 1.96 * se
    )

str(coef_df)

ggplot(coef_df %>% 
    filter(var == "coffee_spending"), 
    aes(x = var, y = estimate)) +
    geom_hline(yintercept = 0, 
               linetype = 'dotted') +
    geom_errorbar(aes(
        ymin = ci_lower_bound,
        ymax = ci_upper_bound
    ), width = 0) +
    geom_point(shape = 21, 
    fill = "white",size = 2) +
        theme_bw() +
            xlab("Estimate") +
            ylab("Estimated effect on hours of sleep") +
            coord_flip() +
            xlab("") +
            ggtitle("Estimated effect of coffee spending on sleep")
    

#### Multiple hypothesis testing ####

## Let's assume that you want to test for 
## a relationship between
## Coffee spending and happiness 

## Let's assume for now that there is 
## actually no relationship
## grad students from above

set.seed(1)

Y <- 2 + 0 * sample + rnorm(100, 0, 10)
plot(sample, Y,
    pch = 16,
    xlab = "Coffee spending", 
    ylab = "Happiness"
)

## What is the true population regression slope here?

## Let's run a regression:

summary(lm(Y ~ coffee_spending))

## Let's assume you have 50 friends, 
## and each friend samples 
## 100 grad students, and then 
## runs the regression

## Let's simulate this

pval_list <- c()

set.seed(1)

for (i in 1:50) {

    ## Draw sample from the population

    X <- rnorm(100, 20, 8)
    error <- rnorm(100, 0, 10)
    h <- 2 + 0 * X + error

    ## run regression

    s <- summary(lm(h ~ X))

    ## Get pval

    s$coefficients[2, 4]

    ## Store pval

    pval_list <- c(pval_list, s$coefficients[2, 4])
}

## Lets look at the distribution of p values

hist(pval_list, breaks = 20)

## Number of p values below 0.05

sum(pval_list < 0.05)






