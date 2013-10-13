library(sm)
library(psych)

kData <- read.table('Stats1.13.HW.02.txt', header = TRUE)

kData.pre <- subset(kData, kData$time == 'pre')
kData.post <- subset(kData, kData$time == 'post')

kData.pre.wm  <- subset(kData.pre,  kData.pre$condition  == 'WM')
kData.post.wm <- subset(kData.post, kData.post$condition == 'WM')
kData.pre.pe  <- subset(kData.pre,  kData.pre$condition  == 'PE')
kData.post.pe <- subset(kData.post, kData.post$condition == 'PE')
kData.pre.ds  <- subset(kData.pre,  kData.pre$condition  == 'DS')
kData.post.ds <- subset(kData.post, kData.post$condition == 'DS')

# Question 1
#
# How many rows of data are in the data file?

nrow(kData)

# Question 2
#
# What is the name of the dependent variable?

names(kData)

# Question 3
#
# What is the mean of SR across all subjects?

mean(kData$SR)

# Question 4
#
# What is the variance of SR across all subjects?

var(kData$SR)

# Question 5
#
# What is the mean of SR for all subjects at pretest?

mean(kData.pre$SR)

# Question 6
#
# What is the standard deviation of SR for all subjects at posttest?

sd(kData.post$SR)

# Question 7
#
# What is the median of SR for all subjects at posttest?

median(kData.post$SR)

# Question 8
#
# Which group has the highest mean at posttest?

describeBy(kData.post, kData.post$condition)

# Question 9
#
# Which one best approximates a normal distribution?

par(mfrow = c(3, 2))

hist(kData.pre.wm$SR)
hist(kData.post.wm$SR)
hist(kData.pre.pe$SR)
hist(kData.post.pe$SR)
hist(kData.pre.ds$SR)
hist(kData.post.ds$SR)

# Question 10
#
# Which group showed the biggest gains in SR?

mean(kData.post.wm$SR) - mean(kData.pre.wm$SR)
mean(kData.post.pe$SR) - mean(kData.pre.pe$SR)
mean(kData.post.ds$SR) - mean(kData.pre.ds$SR)
