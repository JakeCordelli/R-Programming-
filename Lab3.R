####################################################
##		Jake Cordelli
##		CSC	315,	Lab	#3
####################################################
library(readr)
library(ggplot2)

##1
income <- cbind("Not Too Happy" = c(26, 117, 172), "Pretty Happy" = c(233, 473, 383), "Very Happy" = c(164, 293, 132))
rownames(income) <- c("AboveAvg", "Average", "BelowAvg")
income

income.conditional <- prop.table(income, margin = 2)

##There seems to be a correlation between income and happiness


##2
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv")
View(survey)


##3
t <- table(survey$CatOrDogPerson, survey$FavoriteLanguage)
t.conditional <- prop.table(t, margin = 1)

ggplot(survey) + geom_bar(aes(CatOrDogPerson, fill=FavoriteLanguage), position = "fill") +
  labs(x = "", y = "Relative frequency", title = "Cat or Dog Preference")
##There is an association between dogs and having a diverse programming language 
##population. As for cat, it is mainly associated with C++. 

##4
HS.GPA <- survey$HS.GPA
College.GPA <- survey$College.GPA

ggplot()  + geom_point(aes(HS.GPA, College.GPA)) +
  theme_classic() + 
  labs(x = "HS.GPA (%)", y = "College.GPA (%)",
       title = "Highschool GPA vs College GPA")

##5
cor(HS.GPA, College.GPA)
##According to the correlation value of about .882,
##there seems to be a strong association between HS and College GPAs


##6
fit = lm(College.GPA ~ HS.GPA)
fit
##The equation of the regression line is y=.99646x-.01443
##This means that, starting at -.01443, the value of y increases at a 
##rate of .99646 units for every one unit increase in the x value.


##7
predict(fit)
predict(fit, data.frame(HS.GPA=3.0))
##Based on a HS GPA of 3.0, it is predicted that a student will obtain a 
##2.97 in college 


##8
weight <- mtcars$wt
mpg <- mtcars$mpg

ggplot(data = NULL, aes(weight, mpg)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "weight (thousands lbs)", y = "mpg",
       title = "weight vs mpg") +
  geom_smooth(method = "lm", color = "mediumvioletred")

##There is a heavy relationship between weight and mpg according to
##the regression model. 


##9
fit = lm(mpg ~ weight)
fit
##The equation of the line is y=-5.344x+37.285. This translates into
##for every 1000 pound increase in car weight, it is expected that there
##will also be a 5.344 decrease in mpg.


##10
predict(fit)
predict(fit, data.frame(weight=3000))
##I am not going to predict the milage of a car that weighs
##7000 pounds because that would be an extrapolation

