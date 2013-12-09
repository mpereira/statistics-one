# Recent theories in the field of cognitive enhancement suggest that people's
# belief about whether or not cognitive abilities can be improved influences the
# outcome of a training program. In this week's assignment, we take a look at a
# dataset including two different kinds of feedback given to participants in a
# cognitive training program, either fixed (cognitive abilities are innate and
# cannot be improved) or malleable (cognitive abilities are largely driven by
# experiences). DVs include verbal, spatial, and intelligence measures, provided
# before and after training.

library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

kData          <- read.table('Stats1.13.HW.11.txt', header = T)
data           <- kData
data.fixed     <- subset(kData, kData$cond == 'fixed')
data.malleable <- subset(kData, kData$cond == 'malleable')

# Question 1
#
# Using a t-test, compare verbal scores before and after training in the fixed
# condition. Is the difference pre-test to post-test significant?

t.test(data.fixed$verbal.pre, data.fixed$verbal.post, paired = T)

# Question 2
#
# What are the degrees of freedom for the comparison between pre-test and
# post-test for the spatial scores?

t.test(data.fixed$spatial.pre, data.fixed$spatial.post, paired = T)

# Question 3
#
# Run a Wilcoxon test for the same comparison (pre-test to post-test on spatial
# scores, fixed condition). Which of the two tests gives the highest p-value for
# the comparison?

wilcox.test(data.fixed$spatial.pre, data.fixed$spatial.post, paired = T)

# Question 4
#
# What is the effect size (Cohen's d) for the difference between pre-test and
# post-test spatial scores for the malleable condition? (round to two decimal
# places)

round(cohensD(data.fixed$spatial.pre,
              data.fixed$spatial.post,
              method = 'paired'), 2)

# Question 5
#
# Which of the three tasks shows the largest improvements from pre-test to
# post-test, in the fixed condition?

cohensD(data.fixed$verbal.post, data.fixed$verbal.pre, method = 'paired')
cohensD(data.fixed$spatial.post, data.fixed$spatial.pre, method = 'paired')
cohensD(data.fixed$intel.post, data.fixed$intel.pre, method = 'paired')

# Question 6
#
# Which of the three tasks shows the largest improvements from pre-test to
# post-test, in the malleable condition?

cohensD(data.malleable$verbal.post, data.malleable$verbal.pre, method = 'paired')
cohensD(data.malleable$spatial.post, data.malleable$spatial.pre, method = 'paired')
cohensD(data.malleable$intel.post, data.malleable$intel.pre, method = 'paired')

# Question 7
#
# Conduct Mann-Whitney comparisons between all tasks at pre-test. Which task(s)
# differ significantly from the other two in pre-test scores?

wilcox.test(kData$verbal.pre, kData$spatial.pre, paired = F)
wilcox.test(kData$verbal.pre, kData$intel.pre, paired = F)
wilcox.test(kData$spatial.pre, kData$intel.pre, paired = F)

# Question 8
#
# Which feedback condition led to the largest improvements overall?

cohensD(data.fixed$verbal.post, data.fixed$verbal.pre, paired = T)
cohensD(data.fixed$spatial.post, data.fixed$spatial.pre, paired = T)
cohensD(data.fixed$intel.post, data.fixed$intel.pre, paired = T)
cohensD(data.malleable$verbal.post, data.malleable$verbal.pre, paired = T)
cohensD(data.malleable$spatial.post, data.malleable$spatial.pre, paired = T)
cohensD(data.malleable$intel.post, data.malleable$intel.pre, paired = T)

# Question 9
#
# Which task is largely driving this effect?

cohensD(data.fixed$verbal.post, data.fixed$verbal.pre, paired = T)
cohensD(data.fixed$spatial.post, data.fixed$spatial.pre, paired = T)
cohensD(data.fixed$intel.post, data.fixed$intel.pre, paired = T)
cohensD(data.malleable$verbal.post, data.malleable$verbal.pre, paired = T)
cohensD(data.malleable$spatial.post, data.malleable$spatial.pre, paired = T)
cohensD(data.malleable$intel.post, data.malleable$intel.pre, paired = T)

# Question 10
# Based on the present data, are you convinced that malleable feedback is
# beneficial to performance when engaging in a cognitive training program?
