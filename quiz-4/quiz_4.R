# Salary can be influenced by many variables. Among these, years of
# professional experience and total courses completed in college are critical.
# This week we test this hypothesis with a simulated dataset including an
# outcome variable, salary, and two predictors, years of experience and courses
# completed. Here are a few questions based on what was covered in the lectures
# and the lab. Have fun!

library(psych)

kData <- read.table('Stats1.13.HW.04.txt', header = TRUE)
data <- kData

# Question 1
#
# What is the correlation between salary and years of professional experience?

round(cor(kData$salary, kData$years), 2)

# Question 2
#
# What is the correlation between salary and courses completed?

round(cor(kData$salary, kData$courses), 2)

# Question 3
#
# What is the percentage of variance explained in a regression model with salary
# as the outcome variable and professional experience as the predictor variable?

summary(lm(kData$salary ~ kData$years))

# Question 4
#
# Compared to the model from Question 3, would a regression model predicting
# salary from the number of courses be considered a better fit to the data?
#
# ( ) Yes
# (*) No

summary(lm(kData$salary ~ kData$courses))

# Question 5
#
# Now let's include both predictors (years of professional experience and
# courses completed) in a regression model with salary as the outcome. Now what
# is the percentage of variance explained?

summary(lm(kData$salary ~ kData$years + kData$courses))

# Question 6
#
# What is the standardized regression coefficient for years of professional
# experience, predicting salary?

round(cor(kData$salary, kData$years), 2)

# Question 7
#
# What is the standardized regression coefficient for courses completed,
# predicting salary?

round(cor(kData$salary, kData$courses), 2)

# Question 8
#
# What is the mean of the salary distribution predicted by the model including
# both years of professional experience and courses completed as predictors?
# (with 0 decimal places)

data$predictedSalary = fitted(lm(kData$salary ~ kData$years + kData$courses))
mean(data$predictedSalary)

# Question 9
#
# What is the mean of the residual distribution for the model predicting salary
# from both years of professional experience and courses completed? (with 0
# decimal places)

data$predictedSalaryError = resid(lm(kData$salary ~ kData$years + kData$courses))
mean(data$predictedSalaryError)

# Question 10
#
# Are the residuals from the regression model with both predictors normally
# distributed?
#
# (*) Yes
# ( ) No

hist(data$predictedSalaryError)
