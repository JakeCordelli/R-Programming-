library(ggplot2)

####################################################################
## Percentiles and Boxplots
####################################################################

percentile <- function(x, val) {
   x = x[!is.na(x)]  ## remove missing values
   ans = sum(x <= val) / length(x) * 100
   return(ans)
}

x <- 1:10

############################################
# What is the percentile of the value 6.5?
############################################
percentile(x,6.5)

############################################
# Visualization of the 60th percentile
############################################

# we will plot x-values (the data) vs. y = 1 for visualization
x <- 1:10
y <- rep(1,10)

# set the colors
v.red <- rep("red", 6)
v.blue <- rep("blue", 4)
color <- c(v.red, v.blue)

# use ggplot to plot points; note that we do not have a data.frame, so 
# we do not pass any values to ggplot(); the x- and y-vectors can
# be specified directly for the aesthetics
ggplot() + geom_point(aes(x,y), color = color) +
           theme_classic() +
           geom_vline(xintercept = 6.5) +
           ylim(0.6,1.4) +
           annotate("text", x = 3, y = 1.3, label = "60% of the data", color = "red") +
           annotate("text", x = 9, y = 1.3, label = "40% of the data", color = "blue") +
           ggtitle("Visualization of the 60th percentile")
           

#############################################
# find the 20th and 30th percentiles
#############################################
quantile(x, .2)   ## finds the 20th percentile
quantile(x, .3)   ## finds the 30th percentile
quantile(x, c(.2,.3)) ## finds both the 20th and 30th percentile

#############################################
# Quartiles and 5-number summary 
# (min, Q1, Q2, Q3, max)
# summary/quantile and fivenum give different
# values of Q1 and Q3 when there are even 
# numbers of observations
# However both are correct
#############################################

summary(x) ## note that this also contains the mean
quantile(x) ## just the quantiles
fivenum(x)  ## five number summary

#############################################
# Quartiles are the same for odd number of
# observations
#############################################
z = 1:11
summary(z)
quantile(z)
fivenum(z)

#############################################
# boxplot - draw box around Q1 and Q3
# draw fence corresonding to Q1 - 1.5*IQR
# and Q3 + 1.5*IQR
#############################################

# create a new vector for the data
numbers <- c(1:10, 20:21)

#################################################################
# Generate boxplot using ggplot
# Note: since we do not have a data.frame, we include the data 
# directly in the aesthetic in the following format:
#     "." - used to indicate that there are no 'x' values (groups) 
#         to plot
#     numbers  - the y-values to plot
# Also note the 'fill' argument is outside the aesthetic, so it 
#     is applied to all groups
###################################################################
ggplot() + geom_boxplot(aes(".", numbers), fill = "lightblue") + xlab("")  +
           ggtitle("Example boxplot") + theme_classic()
  

#####################################################################
# side-by-side boxplots can be used to compare quantitiave data 
# across two groups 
#####################################################################

# Let's compare the heights of males and females using side-by-side boxplots #
# males = 0, females = 1
heights = read.csv("http://pastebin.com/raw/g7UdTFKG")

# since gender is categorical, let's change it to a factor 
# (othrewise, ggplot will treat gender as numeric)
heights$GENDER = factor(heights$GENDER)
levels(heights$GENDER) = c("Male", "Female")

# Here the 'fill' argument is part of the aesthetics, since it maps those
# values to a color
bplot <- ggplot(heights) + geom_boxplot(aes(GENDER, HEIGHT, fill = GENDER))
bplot

# Change formatting (remove legend, change labels)
# Note: other options for the legend.position are "left", "right", "bottom", "top"
bplot + theme_classic() + theme(legend.position = "none") +
        ggtitle("Comparison of heights between males and females") +
        labs(x = "gender", y = "height")

