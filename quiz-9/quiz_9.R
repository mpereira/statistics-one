library(psych)
library(car)
library(lsr)

kData              <- read.table('Stats1.13.HW.09.txt', header = T)
data               <- kData
data.Early         <- subset(kData, kData$Haste == 1)
data.OnTime        <- subset(kData, kData$Haste == 2)
data.Late          <- subset(kData, kData$Haste == 3)
data.GoodSamaritan <- subset(kData, kData$Prime == 1)
data.Occupational  <- subset(kData, kData$Prime == 2)

# Question 1
#
# What is the class of Haste and Prime in R?

class(kData$Haste)
class(kData$Prime)

# Question 2
#
# After converting Haste and Prime to factors, run an ANOVA with both Haste and
# Prime as independent variables. Is the effect of Haste significant?

summary(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)))

# Question 3
#
# Is the effect of Prime significant?

summary(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)))

# Question 4
#
# Is the interaction significant?

summary(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)))

# Question 5
#
# Save the ANOVA summary in a table and run Tukey's pairwise comparison on all
# group means. Do each level of Haste significantly differ from one another?

TukeyHSD(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)))

# Question 6
#
# What is the partial eta-squared value for the effect of Haste? (round to 2
# decimal places).

round(etaSquared(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)),
                 anova = T),
      2)

# Question 7
#
# What is the partial eta-squared value for the interaction? (round to 2 decimal
# places).

round(etaSquared(aov(kData$Helping ~ factor(kData$Haste) * factor(kData$Prime)),
                 anova = T),
      2)

# Question 8
#
# Let's now run simple effects of Prime at each level of Haste. At which level
# of Haste is the effect of Prime significant?

summary(aov(data.Early$Helping ~ data.Early$Prime))
summary(aov(data.OnTime$Helping ~ data.OnTime$Prime))
summary(aov(data.Late$Helping ~ data.Late$Prime))

# Question 9
#
# What is the partial eta-squared value for the effect of Prime when people were
# early? (round to 2 decimal places).

round(etaSquared(aov(data.Early$Helping ~ factor(data.Early$Prime)), anova = T), 2)

# Question 10
#
# Which one of the following statements best illustrates the main finding of the
# study?
#
# a) People are more likely to be primed to help others if they are early
# b) All of the above
# c) People are less likely to help others if they are early
# d) People are more likely to help others after being primed to do so if they
#    are early
