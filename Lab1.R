##########################################################################
# CSC-315, Fall 2017
# Lab 1: R programming
# Name: 
##########################################################################

##########################################################################
# Add R code to the script below in order to explicitly output the 
# answers to the following questions. You will turn in this assignment
# by creating a Notebook and turning in a physical copy.
##########################################################################

  
#1) What is 34+29*12
v0<-(34+(29*12))
#Answer: 382


#2) What is the sum of all integers between (and including) -100 and +100.
v1 <- -100:100
sum(v1)
#Answer: 0


#3)	Create a vector that contains the numbers 1-10.
v2<-1:10


#4)	Create a vector that contains the numbers 1-10, 17, and 25.
v3 <- c(v2,17,25)


#5)	Create a vector that contains two elements, your first name and last name
v4 <- list(firstName = "Jake", lastName="Cordelli")

#6) Create a matrix that contains the numbers 87, 89, and 91 in the 1st row and
#   76, 88, and 83 in the second row. Change the column names to "ExamI", "ExamII", and "ExamIII",
#   and change the row names to "Joseph Smith" and "Amy Davis"
m <- matrix(1:6,ncol=3,byrow = TRUE)
m[1]=87
m[2]=89
m[3]=91
m[4]=76
m[5]=88
m[6]=83
colnames(m)<-c("Exam1", "Exam2", "Exam3")
rownames(m)<-c("Joseph Smith", "Amy Davis")


#7) Calculate the average grade for Amy Davis, using the 'mean' function.
v5<-mean(m[1:3])
#Answer: 89

#8) "Joseph" prefers to be called "Joe". Change the name of the 1st row of the matrix 
#   to "Joe Smith" (you should do this in a statement that only changes the name of the first row)
rownames(m)<-c("Joe Smith", "Amy Davis")

#9)	Create a list that includes the following objects: 
#       (1) a vector that contains two elements, your first name and last name; 
#       (2) your age
v6<-c("Jake", "Cordelli")
person <- list(v6, age = 21)


#10)  Read in the survey.txt file as was done in class (and put the code for this in your script!)

# Assign your URL to `url`
survey <- read.delim("https://gdancik.github.io/CSC-315/data/datasets/survey.txt")

#11)  How many of these individuals are male? How many are female?
library(dplyr)
survey.males <- filter(survey, Gender == "Male")
summary(survey.males)
survey.females <- filter(survey, Gender ==0)
summary(survey.females)


#12)	How many individuals surveyed did not use FB (i.e., spent 0 hours / week on FB)
survey.fb <-filter(survey, FB==0)
survey.fb
summary(survey.fb)

#13) What are the GPAs of the three students with the lowest College GPAs? Hint: use the sort function.
#     (Note: your answer to this question should display only these GPAs)
survey.lGPA <- arrange(survey, College.GPA)
survey.lGPA[1:3,]


#14) What are the GPAs of the three students with the highest college GPAs? Hint: use the sort function with decreasing = TRUE
survey.hGPA <- arrange(survey, desc(College.GPA))
survey.hGPA[1:3,]

#15) Create a data.frame called survey.A that includes all students that have a 3.5 college GPA or higher
survey.A <- filter(survey, College.GPA>=3.5)

#16) How many of these individuals are male? How many are female?
summary(survey.A)
                                                                                                                                                                                                                                                                                                                      

