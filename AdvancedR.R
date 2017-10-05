###################################
## functions
###################################

## two arguments ##
divide <-function(x,y) {
    return(x/y)
}
divide(3,4)

## default arguments ##
divide <-function(x,y = 1) {
  return(x/y)
}

### arguments are matched in order unless they are named
divide(3)         # 3 / 1 (since 1 is default y-value)
divide(3,2)       # 3/2 = 1.5
divide(2,3)       # 2/3 = .666
divide(y = 2, 3)  # 3/2 = 1.5

###################################################
# return is implicit if the function ends with
# an expression (but NOT an assignment)
##################################################

# same divide function as above, with implicit return
divide <-function(x,y = 1) {
  x/y
}

divide(3,2) ## 3/2 = 1.5

###################################
## for loops
###################################

# loop with i as index
x <- c(1,3,4)
for (i in 1: length(x)) {
    cat (x[i])
}

# loop for each value of x
for (x in c(1,2,5,7)) {
    cat (x)
}

######################################################################
## Note: Depending on how for loops are set up, their execution can
## be very slow in R, and alternatives should be used if possible, 
## such as apply (for matrices) and lapply or sapply (for lists), 
## which are described below
#####################################################################


######################################################################
## apply - applies a function to a row (MARGIN = 1) 
## or column (MARGIN = 2) of a matrix
######################################################################

###########################################
# example: find max of each row of 'm'
###########################################
m <- matrix(1:20, ncol=5, byrow=TRUE)
rowMaxes <- apply(m, 1, max)
rowMaxes

###############################################################
## add the 'n' smallest numbers in a vector
## Note: could also return sum(sort(x)[1:n])
###############################################################
add.smallest <- function(x, n=2) {
  x.sorted <- sort(x)
  sum(x.sorted[1:n])
}
ans.row <- apply(m, 1, add.smallest) ## for each row
ans.row

ans.col <- apply(m, 2, add.smallest) ## for each col
ans.col

ans.row.4 <- apply(m, 1, add.smallest, n = 4)
ans.row.4

## alternative using an inline function
ans.row <- apply(m, 1, function(x) sum(sort(x)[1:2]))

## lapply applies a function to each object in a list
person <- list(name = "Bob", sibling.ages = c(43,21), pet.ages = c(8,3))
lapply(person, length)

## sapply does the same but returns the results in a vector
sapply(person, length)

########################################
# if statements - follows C/C++/Java format
########################################
compare <-function(x, ref = 0) {
  if (x < ref) {
    cat("the number", x, "is less than", ref, "\n")
  } else if (x > ref) {
    cat("the number", x, "is greater than", ref, "\n")
  } else {
    cat("the number", x, "is equal to", ref, "\n")
  }
}

compare(5, 3)

#######################################################################
# Question set (use the grades matrix below):
#######################################################################

grades <- matrix(c(71,86,82,93,87,92,85,85,98,99,100,92),ncol=3,byrow=T)
rownames(grades) <- c("Steve", "Joe", "Jane", "Andrea")

# 1. Using the apply function, find the mean grade for each student, and
#    the mean grade for each assignment


# 2. The function below calculates the average of a vector of numbers
# and returns TRUE if the average is >= 90 (i.e., in the A range)
# Using the the is.A function below, and 'apply', identify the names of
# the students with an A average. Can you write code that only outputs the
# names of the students with As?

is.A <-function(x) {
 mean(x) >= 90    # this TRUE/FALSE value is implicitly returned
}

# 3. Write a function that takes a vector of numbers, and returns 
#    an 'A' if the mean grade >= 90
#    otherwise a 'B' if the mean grade >= 80
#    otherwise a 'C' if the mean grade >= 70
#    otherwise a 'D' if the mean grade >= 60
#    otherwise an 'F' grade

# 4. Use this function and 'apply' to find the grade for each individual


