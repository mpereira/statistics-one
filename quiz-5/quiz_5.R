library(psych)

kData <- read.table('Stats1.13.HW.04.txt', header = TRUE)

# Question 1
#
# Run a regression model with salary as the outcome variable and years of
# experience as the predictor variable. What is the 95% confidence interval for
# the regression coefficient? Type your answer exactly as it appears in R but
# include only two decimal places (for example, if the 95% confidence interval
# is -1 to +1 then type -1.00 1.00)

round(confint(lm(kData$salary ~ kData$years)), 2)

# Question 2
#
# Run a regression model with salary as the outcome variable and courses as the
# predictor variable. What is the 95% confidence interval for the regression
# coefficient?

round(confint(lm(kData$salary ~ kData$courses)), 2)

# Question 3
#
# Run a multiple regression model with both predictors and compare it with both
# the model from Question 1 and the model from Question 2. Is the model with
# both predictors significantly better than:

anova(lm(kData$salary ~ kData$years), lm(kData$salary ~ kData$years + kData$courses))
anova(lm(kData$salary ~ kData$courses), lm(kData$salary ~ kData$years + kData$courses))

# Question 4
#
# Run a standardized multiple regression model with both predictors. Do the
# confidence interval values differ from the corresponding unstandardized model?

confint(lm(kData$salary ~ kData$years + kData$courses))
confint(lm(scale(kData$salary) ~ scale(kData$years) + scale(kData$courses)))

# Question 5
#
# What function could you use to take a random subset of the data?

sample

# Question 6
#
# Run the following command in R: set.seed(1). Now take a random subset of the
# original data so that N=15. Is the correlation coefficient between salary and
# years of experience in this sample higher or lower than in the whole data set?

set.seed(1)
kDataSampleSize15 <- kData[sample(nrow(kData), 15), ]
cor(kData$salary, kData$years)
cor(kDataSampleSize15$salary, kDataSampleSize15$years)

# Question 7
#
# Take a subset of the original data from row 51 to 70. What is the percentage
# of variance explained by a multiple regression model with both predictors
# (Provide your result with no decimal place)

kDataFrom51To70 <- kData[51:70, ]
summary(lm(kDataFrom51To70$salary ~ kDataFrom51To70$years + kDataFrom51To70$courses))

# Question 8
#
# Using model comparison, which model provides the best fit for the subsetted
# data from Question 7?

model1 <- lm(kDataFrom51To70$salary ~ kDataFrom51To70$years)
model2 <- lm(kDataFrom51To70$salary ~ kDataFrom51To70$courses)
model3 <- lm(kDataFrom51To70$salary ~ kDataFrom51To70$years + kDataFrom51To70$courses)
anova(model1, model2)
anova(model1, model3)
anova(model2, model3)

# Question 9
#
# What is the correlation between the salary values predicted by the multiple
# regression model and the actual salary scores in the subsetted data? (Provide
# your result rounded to 2 decimal places)

dataFrom51To70 <- kDataFrom51To70
dataFrom51To70$predictedSalary <-
  fitted(lm(dataFrom51To70$salary ~ dataFrom51To70$years + dataFrom51To70$courses))
round(cor(dataFrom51To70$predictedSalary, dataFrom51To70$salary), 2)

# Question 10
#
# Compute the correlation between the scores predicted by the multiple
# regression model and the residuals from the same model. Is the correlation
# statistically significant?

data <- kData
data$predictedSalary <- fitted(lm(data$salary ~ data$years + data$courses))
data$predictedSalaryError = resid(lm(data$salary ~ data$years + data$courses))
round(cor(data$predictedSalary, data$predictedSalaryError), 2)
