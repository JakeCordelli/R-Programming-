########################################################
# Name:Jake Cordelli
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################

library(ggplot2)
##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions

# Note: all graphs must be given an appropriate title, x-axis label, and
#   y-axis label
##########################################################################

# 1.load our classes survey data 
#   (available at https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv)
#   and add the code for this to the script. 
survey_fall_2017 = read.csv("https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv")

# 2. How many students completed the survey?
nrow(survey_fall_2017)

# 3. How many questions were asked (i.e., how many columns are there)?
ncol(survey_fall_2017)

# 4. Construct a frequency bar graph for the response to "Are you a cat or a dog person?"
#    Remove the legend by adding the following component to the end of your 
#    ggplot() code: theme(legend.position = "none")
COD <- (survey_fall_2017$CatOrDogPerson)
CODTable<-table(COD) # frequency table

survey_fall_2017$CatOrDogPerson = factor(survey_fall_2017$CatOrDogPerson)
levels(survey_fall_2017$CatOrDogPerson) = c("Cat", "Dog")



barplot(CODTable, main ="Title",
        xlab="Animal Type",
        col=c("violetred1", "cyan"))

counts = data.frame(CODTable)
ggplot(CODTable) + geom_col(aes(x=COD, y=Freq, fill = COD)) +
  ggtitle("Title") +
  labs(x = "Animal Type", y = "Frequency")

survey_fall_2017 = read.csv("https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv")



# 5. Construct a relative frequency table for favorite programming language
FPL <-(survey_fall_2017$FavoriteLanguage)
FPLTable<-table(FPL)

FPLTable / length(FPL) #relative frequency (proportions) 
counts1 = data.frame(FPLTable)

# 6. Construct a Pareto Chart for favorite programming language
counts = data.frame(FPLTable)

# reorder based on counts (use negative value to order from high to low
counts$FPL <- reorder(counts$FPL, -counts$Freq)

ggplot(counts) + geom_col(aes(x=FPL, y=Freq, fill = FPL)) +
  ggtitle("Fascinating Data") +
  labs(x = "Favorite Language", y = "Frequency")

# 7. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week).

# create new table of females showing only GPAs -- approach 1
Alcoholics <- survey_fall_2017$Alcohol>0
AlcoholTable<-table(Alcoholics)
AlcoholTable / length(survey_fall_2017$Alcohol) #relative frequency (proportions) 
pie(AlcoholTable)

# 8. Out of the "Cat" people in this class, what proportion list "Python" as their 
#    favorite programming language? Answer this question by first creating a new
#    data.frame for "Cat" people only. Then generate a relative frequency table
#    for favorite programming language. Then answer the same question for "Dog" 
#    people. What do you conclude about programming language preference (for 
#    students in this class) based on this data?

CatPeople<-(survey_fall_2017$CatOrDogPerson == 'Cat')
CatPeople
d.status <- data.frame(status = CatPeople)
ggplot(d.status, aes(x=status)) + geom_bar(aes(fill = status)) +
  ggtitle("Cat people (People who like cats)") +
  labs(x = "0 or 1", y = "Frequency")

PyCats<-c(d.status == 'python')
PyCats

DogPeople<-(survey_fall_2017$CatOrDogPerson == 'Dog')
DogPeople
d.status <- data.frame(status = DogPeople)
ggplot(d.status, aes(x=status)) + geom_bar(aes(fill = status)) +
  ggtitle("Dog people (People who like dogs)") +
  labs(x = "0 or 1", y = "Frequency")

          #This is outputting zero values for PyDogs(Dog people who like python)
PyDogs<-c(d.status == 'Python')
PyDogs



# 9. Construct a histogram for Alchol consumption, by using the hist() function with the argument
#    breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#    Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?
plot.hist <- function(x, breaks = 14, ...) {
  hist(x, breaks = breaks, ...)
  abline(v = mean(x), col = "blue")
  abline(v = median(x), col = "red")
  text(mean(x), 1, "mean", srt = 90)
  text(median(x), 1, "median", srt = 90)  
}
alcoholHistData <- survey_fall_2017$Alcohol
plot.hist(alcoholHistData, xlab = "Alcohol Consumption", main = "Histogram of Alcohol")
  #The data is unimodal and is skewed right

# 10. Calculate the mean and median for Alcohol consumption. 
#    Which is a better measure of averages? (Note: although these numbers are similar,
#    one would still be considered better than the other -- why?)
mean(alcoholHistData)
median(alcoholHistData)
                  #The mean is a better number to follow because 1 is a discrete number 
                  #used in this context to represent a discrete interval. 

# 11. What is the 75th percentile for HS GPA??
x<-survey_fall_2017$HS.GPA
percentile <- function(x, val) {
  x = x[!is.na(x)]  ## remove missing values
  ans = sum(x <= val) / length(x) * 100
  return(ans)
}
quantile(x, .75)

# 12. Ten percent of indivduals have HS GPAs above what value?
quantile(x, .90)

# 13. Create side-by-side boxplots showing the College GPA based on
#     a person's favorite programming language.  Does there appear 
#     to be a significant difference in the GPAs between these 
#     groups? Are there any outliers? If so, how many?


info = read.csv("https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv")

info$FavoriteLanguage = factor(info$FavoriteLanguage)
levels(info$FavoriteLanguage) = c("C++", "Java", "Other", "Python")

bplot <- ggplot(info) + geom_boxplot(aes(FavoriteLanguage, College.GPA, fill = FavoriteLanguage))
bplot

# Change formatting (remove legend, change labels)
# Note: other options for the legend.position are "left", "right", "bottom", "top"
bplot + theme_classic() + theme(legend.position = "none") +
  ggtitle("Comparison of GPA based on favorite Programming Language") +
  labs(x = "Language", y = "GPA")


# 14. For college GPA, what is the variance and standard deviation?
x <- info$College.GPA
plot.deviations <- function(x) {
  plot(x, 1:length(x), xlab = "observed value", ylab = "index", pch = 19)
  m = mean(x)
  abline(v = m, col = "red", lwd = 2)
  for (i in 1:length(x)) {
    p1 = c(x[i],m)
    p2 = c(i,i)
    lines(p1,p2, col = "blue")
  }
}
plot.deviations(x)
dev <- x - mean(x)

sd.table = data.frame(x = x, dev = dev, "squared deviation" = dev**2)
sd.table
n = nrow(sd.table)

sum.squared.dev <- sum(sd.table$squared.deviation)
x.var.manual <- (sum.squared.dev) / (n-1)
x.sd.manual <- sqrt(x.var.manual)

##################################################
# easy way to calculate variance and standard 
# deviation in R
##################################################
x.var <- var(x)
x.sd <- sd(x)  


# 15. Create a vector with 4 values that has a standard deviation of 0.
z<-c(0, 0, 0, 0)
z.sd <- sd(z) 
z.sd

