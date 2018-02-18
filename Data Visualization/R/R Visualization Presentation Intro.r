#After RStudio intro and reading in data.

#Going to assume a basic familiarity with R, but going to go over a few basics
#in case anyone is completely new.


# There are a number of different foundational syntax elements in R. Three of the
# most important are objects, expressions, and functions.

# Objects are the most basic element of R. Everything R does is manipulating objects.
# These objects can have different properties. For instance, a dataset is an object
# that is made up of a row/column structure of data entries. A new variable we create
# would be an object according to R. These objects can be created and changed, and
# objects can contain other objects.

x <- 5

x
x <- 6
# x

age <- 45
weight <- 223.45

# What is the assignment arrow?

# In most syntax based languages, there is an operator that indicates the creation
# of a variable/object. In R, this is the assignment arrow, or "<-".

# The assignment arrow can be thought of as similar to an equals sign ("="), where whatever
# is to the right of the arrow is assigned to what is on the left side of the arrow.

# So, for a simple example, we can use the assignment arrow to create a copy of the
# object 'x' we created earlier:

new.x <- x
someothervariabe <- x


# Again, the object x on the right of the arrow, is being assigned to the new object
# named "new.x", on the left of the arrow.


# The assignment arrow can be used for lots of different things, including saving the
# results of analyses and modifying datasets.


# Expressions are statements that are processed by R. For instance, a math-based
# expression might be

2 * (5+10)

# Typing this into R will result in "30". Expressions can be assigned to objects
# or used just one time, as in the example above.


tenure1 <- 2 * (5+10)

tenure1 <- tenure1 * 12

tenure1 <- tenure1 - x

tenure2 <- 20

tenure_total <- tenure1 + tenure2


# Functions are similar to expressions, but are generally used to run a series of
# R commands. Through its base package and add-on packages, R has access to thousands
# of functions, or you can make your own.

sum(tenure1, tenure2)
max(3,4,5,7,88,0, -7)

# Addressing rows/columns

# There are a number of ways to call out parts of data. In general, because R is syntax
# based, in order to manipulate data or run functions on certain variables, you need
# to be able to refer to specific parts of the dataset.

# One way of addressing these elements is to use matrix indices. R
# uses standard matrix notation where rows and columns are entered inside brackets.

# For instance, if we want to see the data in Row 10, we can type:

testdata[10, ]

# and if we wanted to see the data in the second column, we can type:

testdata[, 2]

# and these can be combined. For instance, if you want to pull out the value that
# represents the intersection of Row 10 and Column 2, you would type:

testdata[10, 2]

# One of the first things you'll realize about R is that there are many ways of doing
# the same thing. For instance, you can also refer to columns by using the column
# name. This is often easier than trying to remember what the column number is.

# So, if we wanted to return the second column, as in our earlier example, we could
# also use the column name

testdata[, "LastName"]
testdata[, "GPA"]
names(testdata)


# Creating New Variables/Data Manipulation

# As analysts, we often want to go beyond our original data and create new
# variables or transform our data. R can easily do this.

# Examples:

# Starting out, if we wanted to convert the current 4 pt version of GPA to a 12 pt
# scale, we could do that by multiplying the current GPA column by 3

GPA12 <- testdata[, "GPA"] * 3

# If we wanted to compress our data by changing the Last Name to the Last Initial,
# we could do that by using the substr function

LastInitial < substr(testdata[, "LastName"], 1, 1)

# To make sure it worked correctly, we can compare the original column to the new
# variable

testdata[1:5, "LastName"]
LastInitial[1:5]

#Again, this ia very basic quick and dirty introduction to manipulating data in R,
#but should introduce some concepts useful for understanding the rest of the presentation.


