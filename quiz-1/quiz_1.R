# Question 1
#
# What should you type in the R console to install the "car" package?

install.packages('car')

# Question 2
#
# Once you have this package installed, what should you type in the R console to
# load the "car" package?

library(car)

# Question 3
#
# What should you type in the R console to check what packages you have
# installed and loaded on your computer?

search()

# Question 4
#
# What should you type to get help about the "data.frame" function?

?data.frame

# Question 5
#
# Create two vectors, the first one named "numbers" including all natural
# numbers from 1 to 10, and the second one named "words" containing the
# following series:"One", "Two", "Three", "Four", "Five", "Six", "Seven",
# "Eight", "Nine", "Ten". From these two vectors, create a dataframe "nw" with
# each vector as a separate column. What should you type to check the attributes
# of "nw"?

attributes(nw)

# Question 6
#
# What command should you type to get R to return the number "8" from the
# dataframe "nw"?

nw[8, 1]

# Question 7
#
# What command should you type to get R to return the word "eight" from the
# dataframe "nw"?

nw[8, 2]

# Question 8
#
# What should you type to create a matrix "a" comprising all natural numbers
# from 1 to 10, with 2 rows and 5 columns.

a <- matrix(1:10, 2, 5)

# Question 9
#
# Create a vector "x" comprising all natural numbers from 1 to 6 and another
# vector "y" comprising all natural numbers from 5 to 10. What should you type
# to combine them in a matrix of 2 rows and 6 columns?

rbind(x, y)

# Question 10
#
# Create a vector "x" comprising all natural numbers from 1 to 6 and another
# vector "y" comprising all natural numbers from 5 to 10. What should you type
# to combine them in a matrix of 6 rows and 2 columns?

cbind(x, y)
