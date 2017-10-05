# CSC 315, Exam I 
# Name: Jake Cordelli

# set your library path if necessary (and don't forget to run the library commands below)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)

# Directions: Modify this script by adding R code to answer the questions and/or 
# complete the steps below. When you are finished, create a Notebook and submit
# your Notebook through Blackboard (https://easternct.blackboard.com/) 
# using the link provided in the Exams tab. If you are unable to create a Notebook,
# you may submit the R script instead with a 5 point penalty. 


# 1. Create a vector called 'values' that contains the numbers 1-10, and 20-30
values <- c(1:10, 20:30)


# 2. Write a function in R called getFirst which takes a vector x as an argument 
#    and returns the value of the first element.  For example, if x <- c(9,2,10),
#    then getFirst(x) would return 9.
getFirst <-function(x) {
  j<-x[1]
  return (j)
}
getFirst(values)


# The line below reads in a 'cereal' dataset, containing nutrition information for 77 
# cereals. We will be interested in the following columns:
#    type - either C for cold or H for hot
#    calories - the number of calories per serving
#    sugars - the amount of sugar (in grams) per serving
#    shelf - the shelf number where the cereal is stocked, with 1 being the lowest shelf,
#             2 being the middle shelf, and 3 being the highest shelf

# Note: after running this code, you can use View(cereal) to view the table
cereal = read_delim("http://www1.easternct.edu/dancikg/files/2014/09/cereal.xlsx", delim = "\t")
View(cereal)

# 3. Find the mean number of calories for these cereals. Find the mean amount of sugar.
sugarInfo <- cereal$sugars
mean(sugarInfo, na.rm=T)

calorieInfo <-cereal$calories
mean(calorieInfo)


# 4. Find the mean amount of calories for hot cereals.
#filter type of cereal
onlyHot <-cereal[cereal$type == 'H',]
onlyHot
mean(onlyHot$calories)


# 5. Complete the code below to construct side-by-side boxplots showing the sugar content for 
#    hot and cold cereals. Is there an association between cereal type (cold and hot) and 
#    sugar content? If so, describe this association.

ggplot() +
  ggtitle("Sugar content for Cold (C) and Hot (H) cereals")

cereal$type = factor(cereal$type)

bplot <- ggplot(cereal) + geom_boxplot(aes(type, sugars, fill = type))
bplot + theme_classic() + theme(legend.position = "none") +
  ggtitle("Comparison of sugar content to hot or cold cereal.") +
  labs(x = "Cereal Type", y = "Sugars")


# 6. Execute the code below to add the column fatFree to the cereal table, where
#       a value of "Yes" indicates that the cereal is fat free, and "No" indicates
#       that it is not. 

# make sure to set your library path if you haven't done so already
library(dplyr)
cereal <- mutate(cereal, fatFree = (fat == 0) )
cereal$fatFree <- factor(cereal$fatFree, c(FALSE, TRUE), labels = c("No", "Yes"))

# Now complete the code below to generate a stacked bar graph
#       showing the proportion of fat-free cereals that are on each shelf (shelf #1,2,3)

#Does not currently compile
#fatFree<- prop.table(fatFree, margin = 1)
#d <- data.frame(type = rownames(fatFree), fatFree)
#m <- melt(d, "type", variable.name = "presence")

ggplot(cereal, aes(x=fatFree)) + geom_bar(aes(fill = fatFree)) +
  ggtitle("Relative frequency of fat free cereals on each shelf") +
  labs(x = "shelf #", y = "relative frequency")


# 7. Complete the code below to construct a scatterplot that predicts calorie 
#    content from sugar content, and add the corresponding regression line. 

sugars <- cereal$sugars
calorieInfo <- cereal$calories

ggplot(data = NULL, aes(sugars, calorieInfo)) + 
  geom_point() +
  ggtitle("Calorie and Sugar Content of Various Cereals")+
  theme_classic() + 
  labs(x = "Sugars (gramss)", y = "calories",
       title = "sugar vs calories") +
  geom_smooth(method = "lm", color = "mediumvioletred")

ggplot() 



# 8. Find the linear regression line that predicts calories from sugar content.
#    Find and interpret the y-intercept and the slope in the context of this dataset.
cor(sugars, calorieInfo)
fit = lm(calorieInfo ~ sugars)
#Th y intercept is 89.158 and the slope is 2.536



# 9. What would you predict the calorie content to be for a cereal with 10 grams of sugar?
#     What would you predict the calorie content to be for a cereal with 20 grams of sugar?  
#     (Remember, if this prediction would be an extrapolation, you should say so and not 
#     make this prediction). 
predict(fit)
predict(fit, data.frame(sugars=10))
#the predicted calorie content with a cereal containing 10 grams of sugar is 114.5137 calories
#Predicting the calorie content of a cereal with 20 grams of sugar would be an extrapolation 


# Extra Credit. In 1 or 2 statements only, output the median calories, protein, fat, sodium,
# and fiber content of the cereal data
summary(cereal)

