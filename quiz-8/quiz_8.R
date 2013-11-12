library(psych)
library(car)
library(lsr)

kData        <- read.table('Stats1.13.HW.02.txt', header = T)
data         <- kData
data.pre     <- subset(data,    data$time      == 'pre')
data.post    <- subset(data,    data$time      == 'post')
data$gain    <- data.post$SR - data.pre$SR
data.ds      <- subset(data,    data$condition == 'DS')
data.ds.pre  <- subset(data.ds, data.ds$time   == 'pre')
data.ds.post <- subset(data.ds, data.ds$time   == 'post')
data.pe      <- subset(data,    data$condition == 'PE')
data.pe.pre  <- subset(data.pe, data.pe$time   == 'pre')
data.pe.post <- subset(data.pe, data.pe$time   == 'post')
data.wm      <- subset(data,    data$condition == 'WM')
data.wm.pre  <- subset(data.wm, data.wm$time   == 'pre')
data.wm.post <- subset(data.wm, data.wm$time   == 'post')

# Question 1
#
# Using a dependent t-test, is the difference between pre and post-test scores
# significant?

t.test(data.pre$SR, data.post$SR, paired = T)

# Question 2
#
# Create subsets for each training condition. Which group shows no difference
# between pre and post-test scores?

t.test(data.ds.pre$SR, data.ds.post$SR, var.equal = T)
t.test(data.pe.pre$SR, data.pe.post$SR, var.equal = T)
t.test(data.wm.pre$SR, data.wm.post$SR, var.equal = T)

# Question 3
#
# Which training group shows the largest effect size for the difference pre-test
# to post-test?

t.test(data.ds.pre$SR, data.ds.post$SR, var.equal = T)
t.test(data.pe.pre$SR, data.pe.post$SR, var.equal = T)
t.test(data.wm.pre$SR, data.wm.post$SR, var.equal = T)

# Question 4
#
# Reshape the data into a wide format, and create a new variable for gain score.
# Now subset the new dataframe based on the training conditions. Which
# comparison between training conditions does not show a significant difference?

TukeyHSD(aov(data$gain ~ data$condition))

# Question 5
#
# To compare the gain scores across all groups, we now turn to ANOVA. Is the
# homogeneity of variance assumption violated?

leveneTest(data$gain, data$cond, center = 'mean')

# Question 6
#
# Run an ANOVA model on the gain scores as a function of training condition. Is
# the effect of condition significant?

summary(aov(data$gain ~ data$condition))

# Question 7
#
# What is the corresponding eta-squared value? (round to 2 decimal places)

round(etaSquared(aov(data$gain ~ data$condition), anova = T), 2)

# Question 8
#
# Are the eta-squared and partial eta-squared value different in this case?

round(etaSquared(aov(data$gain ~ data$condition), anova = T), 2)

# Question 9
#
# Let's now run post-hoc comparisons (Tukey HSD). Which two groups do not
# significantly differ from one another when considering gain scores?

TukeyHSD(aov(data$gain ~ data$condition))

# Question 10
#
# Based on these data, which training condition should you choose to target some
# improvements in spatial reasoning?

cohensD(data.ds.pre$SR, data.ds.post$SR, method = 'paired')
cohensD(data.pe.pre$SR, data.pe.post$SR, method = 'paired')
cohensD(data.wm.pre$SR, data.wm.post$SR, method = 'paired')
