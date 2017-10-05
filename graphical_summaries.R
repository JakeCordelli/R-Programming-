library(dplyr)
library(ggplot2)

####################################################################
## Graphical summaries of data
####################################################################

status <- c("freshman", "freshman", "sophomore", "sophomore", "junior", "sophomore")

#################################################
# frequency table and relative frequency tables #
# when there are NO missing values
#################################################
table(status) # frequency table
table(status) / length(status) #relative frequency (proportions) 

##################################################################
# pie charts show a slice of pie proportional to each frequency
##################################################################
pie(table(status))


##################################################################
# Look at ggplot2.R before proceeding
##################################################################


##################################################################
# bar graphs construct bars whose heights correspond to 
# frequencies or relative frequencies
##################################################################

# To generate a frequency bar graph from the far data, use
# the 'geom_bar' layer. Note that the aesthetics must
# be specified in the geom_bar layer (not in ggplot)

d.status <- data.frame(status = status)
ggplot(d.status, aes(x=status)) + geom_bar(aes(fill = status)) +
           ggtitle("Class status of students") +
            labs(x = "Class status", y = "Frequency")


# To generate a frequency bar graph from the counts, use
# the 'geom_col' layer.

t = table(status)
counts = data.frame(t)

ggplot(counts) + geom_col(aes(x=status, y=Freq, fill = status)) +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Frequency")


## relative frequency bar graph using ggplot, based on counts
counts <- mutate(counts, prop = Freq/sum(Freq))

ggplot(counts, aes(x=status, y=prop)) + 
  geom_col(aes(fill = status)) +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Relative frequency")


##################################################################
# Construct a Pareto chart to show bars from tallest to shortest
##################################################################

# bars are ordered in alphabetical (factor-level) order by default
levels(counts$status)

# reorder based on counts (use negative value to order from high to low
counts$status <- reorder(counts$status, -counts$Freq)

ggplot(counts,aes(x=status, y=Freq)) + 
  geom_col(aes(fill = status)) +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Frequency")

##################################################################
# Let's look at the survey data (from a prior class)
# that we analyzed previously
##################################################################

survey <- read.delim("http://pastebin.com/raw/1csmBawE")

###################################################################         
# 1. Construct a relative frequency table for the number of males
#     and females
# 2. Construct a bar graph for the proportion of individuals that 
#    agree or disagree that same-sex marriage should be legal in
#    all 50 states. The title of the chart should be "Support of
#     same sex marriage in all 50 states?"
###################################################################


###################################################################
# Histograms are like bar graphs for quantitative variables
# The height of the bar corresponds to the number 
# or proportion of observations that fall within
# a range of values
###################################################################

## we will use the 'base' hist function, rather than the ggplot one
hist(survey$FB, main = "histogram of FB usage", xlab = "Hours/week on FB")

###########################################################
# histogram shapes
# unimodal = 1 mound or mode
# bimodal = 2 mounds or modes
# skewed left: tail is longer (or fatter) on the left
# symmetric: tails are approximately the same length
# skewed right: tail is longer (or fatter) on the right

# Note regarding R code below: you do not need to 
# understand the R code at this point, but you do need to
# be able to distinguish between histogram shapes
###########################################################

x.norm = rnorm(500)
x.right = c(x.norm, runif(10,3,10))
x.left = c(x.norm, runif(10,-10,-3))
par(mfrow = c(1,3))
hist(x.left, main = "left-skewed")
hist(x.norm, main = "symmetric")
hist(x.right, main = "right-skewed")

par(mfrow = c(1,1))  # reset display for 1 image
bimodal = c(rnorm(500, mean = 70), rnorm(500, mean = 65))
hist(bimodal, main = "bimodal distribution")

