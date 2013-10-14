library(psych)

kData <- read.table('Stats1.13.HW.03.txt', header = TRUE)
kData.aer <- subset(kData, kData$cond == 'aer')
kData.des <- subset(kData, kData$cond == 'des')

# Question 1
#
# What is the correlation between S1 and S2 pre-training?

cor(kData$S1.pre, kData$S2.pre)

# Question 2
#
# What is the correlation between V1 and V2 pre-training?

cor(kData$V1.pre, kData$V2.pre)

# Question 3
#
# With respect to the measurement of two distinct constructs, spatial reasoning
# and verbal reasoning, the pattern of correlations pre-training reveals:
#
# ( ) Convergent validity
# ( ) Divergent validity
# (*) Both
# ( ) Neither

cor(kData$S1.pre, kData$S2.pre)
cor(kData$V1.pre, kData$V2.pre)

cor(kData$S1.pre, kData$V1.pre)
cor(kData$S1.pre, kData$V2.pre)
cor(kData$S2.pre, kData$V1.pre)
cor(kData$S2.pre, kData$V2.pre)

# Question 4
#
# Correlations from the control group could be used to estimate test/retest
# reliability. If so, which test is most reliable?
#
# ( ) S1
# ( ) S2
# ( ) V1
# (*) V2

cor(kData.aer$S1.pre, kData.aer$S1.post)
cor(kData.aer$S2.pre, kData.aer$S2.post)
cor(kData.aer$V1.pre, kData.aer$V1.post)
cor(kData.aer$V2.pre, kData.aer$V2.post)

# Question 5
#
# Does there appear to be a correlation between spatial reasoning before
# training and the amount of improvement in spatial reasoning?
#
# ( ) Yes
# (*) No

cor(kData$S1.pre, kData$S1.post - kData$S1.pre)
cor(kData$S2.pre, kData$S2.post - kData$S2.pre)

# Question 6
#
# Does there appear to be a correlation between verbal reasoning before training
# and the amount of improvement in verbal reasoning?
#
# ( ) Yes
# (*) No

cor(kData$V1.pre, kData$V1.post - kData$V1.pre)
cor(kData$V2.pre, kData$V2.post - kData$V2.pre)

# Question 7
#
# Which group exhibited more improvement in spatial reasoning?
#
# ( ) aer
# (*) des

mean(kData.aer$S1.post) - mean(kData.aer$S1.pre)
  + mean(kData.aer$S2.post) - mean(kData.aer$S2.pre)

mean(kData.des$S1.post) - mean(kData.des$S1.pre)
  + mean(kData.des$S2.post) - mean(kData.des$S2.pre)

# Question 8
#
# Create a color scatterplot matrix for all 4 measures at pre-test. Do the
# scatterplots suggest two reliable and valid constructs?
#
# (*) Yes
# ( ) No

pairs(~kData$S1.pre + kData$S2.pre + kData$V1.pre + kData$V2.pre)

# Question 9
#
# Create a color scatterplot matrix for all 4 measures at post-test. Do the
# scatterplots suggest two reliable and valid constructs?
#
# (*) Yes
# ( ) No

pairs(~kData$S1.post + kData$S2.post + kData$V1.post + kData$V2.post)

# Question 10
#
# What is the major change from pre-test to post-test visible on the color
# matrix?

# (*) Variance
# ( ) Correlation coefficients
# ( ) Construct validity
# ( ) All of the above
