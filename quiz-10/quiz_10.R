library(psych)
library(aod)
library(QuantPsyc)

kData           <- read.table('Stats1.13.HW.10.txt', header = T)
data            <- kData
data.ActNow     <- subset(kData, kData$change == 1)
data.WaitAndSee <- subset(kData, kData$change == 0)

# Question 1
#
# What is the median population age for the countries which voted to take action
# against global warming? (round to 2 decimal places)

round(mean(data.ActNow$age), 2)

# Question 2
#
# Run a logistic regression including all predictor variables. Which predictors
# are significant in this model?

summary(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
            family = binomial))

# Question 3
#
# What does the negative value for the estimate of educ means?

# Answer: That it is negatively correlated with wanting to act.

# Question 4
#
# What is the confidence interval for educ, using profiled log-likelihood?
# (round to 2 decimal places, and give the lower bound first and the upper bound
# second, separated by a space)

round(confint(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
                  family = binomial)),
      2)

# Question 5
#
# What is the confidence interval for age, using standard errors? (round to 2
# decimal places, and give the lower bound first and the upper bound second,
# separated by a space)

round(confint.default(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
                          family = binomial)),
      2)

# Question 6
#
# Compare the present model with a null model. What is the difference in
# deviance for the two models? (round to 2 decimal places)

round(with(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
               family = binomial),
           null.deviance - deviance),
      2)

# Question 7
#
# How many degrees of freedom are there for the difference between the two
# models?

with(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
         family = binomial),
     df.null - df.residual)

# Question 8
#
# Is the p-value for the difference between the two models significant?

with(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
         family = binomial),
     pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE))

# Question 9
#
# Do chi-squared values differ significantly if you drop educ as a predictor in
# the model?

wald.test(b = coef(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
                       family = binomial)),
          Sigma = vcov(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
                           family = binomial)),
          Terms = 2)

# Question 10
#
# What is the percentage of cases that can be classified correctly based on our
# model?

ClassLog(glm(kData$change ~ kData$age + kData$educ + kData$gdp + kData$co2,
             family = binomial),
         kData$change)
