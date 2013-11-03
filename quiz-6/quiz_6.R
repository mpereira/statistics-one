library(psych)

kData <- read.table("Stats1.13.HW.06.txt", header = T)
profession.code <- C(kData$profession, treatment)

# Question 1
#
# In a model predicting salary, what is the unstandardized regression
# coefficient for years, assuming years is the only predictor variable in the
# model?

lm(kData$salary ~ kData$years)

# Question 2
#
# In a model predicting salary, what is the 95% confidence interval for the
# unstandardized regression coefficient for years, assuming years is the only
# predictor variable in the model?

round(confint(lm(kData$salary ~ kData$years)), 0)

# Question 3
#
# In a model predicting salary, what is the unstandardized regression
# coefficient for years, assuming years and courses are both included as
# predictor variables in the model?

lm(formula = kData$salary ~ kData$years + kData$courses)

# Question 4
#
# In a model predicting salary, what is the 95% confidence interval for the
# unstandardized regression coefficient for years, assuming years and courses
# are both included as predictor variables in the model?

round(confint(lm(formula = kData$salary ~ kData$years + kData$courses)), 0)

# Question 5
#
# What is the predicted difference in salary between Doctors and Lawyers
# assuming an equal and average number of years and courses?

summary(lm(kData$salary ~ kData$years + kData$courses + profession.code))

# Question 6
#
# Is the predicted difference between Doctors and Lawyers statistically
# significant?

summary(lm(kData$salary ~ kData$years + kData$courses + profession.code))

# Question 7
#
# What is the predicted difference in salary between Doctors and Teachers
# assuming an equal and average number of years and courses?

summary(lm(kData$salary ~ kData$years + kData$courses + profession.code))

# Question 8
#
# Is the predicted difference between Doctors and Teachers statistically
# significant?

summary(lm(kData$salary ~ kData$years + kData$courses + profession.code))

# Question 9
#
# What is the actual difference in mean salary between Doctors and Teachers?

tapply(kData$salary, kData$profession, mean)

# Question 10
#
# What combination of predictors represents the best model in terms of
# predicting salary?

summary(lm(kData$salary ~ kData$years + kData$courses))
summary(lm(kData$salary ~ kData$years + profession.code))
summary(lm(kData$salary ~ kData$courses + profession.code))
summary(lm(kData$salary ~ kData$years + kData$courses + profession.code))
