library(psych)
library(multilevel)

kData <- read.table("Stats1.13.HW.07.txt", header = T)

# Question 1
#
# What is the correlation between extraversion and happiness?

cor(kData$extra, kData$happy)

# Question 2
#
# What is the correlation between extraversion and diversity of life experience?

cor(kData$extra, kData$diverse)

# Question 3
#
# What is the correlation between diversity of life experience and happiness?

cor(kData$diverse, kData$happy)

# Question 4
#
# What percentage of variance in happiness is explained by extraversion?

summary(lm(kData$happy ~ kData$extra))

# Question 5
#
# What percentage of variance in happiness is explained by a model with both
# extraversion and diversity of life experience as predictors?

summary(lm(kData$happy ~ kData$extra + kData$diverse))

# Question 6
#
# What is the 95% confidence interval for the regression coefficient for
# extraversion when it is the only predictor of happiness?

confint(lm(kData$happy ~ kData$extra))

# Question 7
#
# What is the 95% confidence interval for the regression coefficient for
# extraversion when it and diversity of life experience are both predictors of
# happiness?

confint(lm(kData$happy ~ kData$extra + kData$diverse))

# Question 8
#
# What is the unstandardized regression estimate of the indirect effect?

sobel(kData$extra, kData$diverse, kData$happy)

# Question 9
#
# What is the z-value of the Sobel test?

sobel(kData$extra, kData$diverse, kData$happy)

# Question 10
#
# Do these analyses suggest full mediation, partial mediation, or no mediation?

sobel(kData$extra, kData$diverse, kData$happy)
