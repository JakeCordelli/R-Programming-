##############################################
# associations.R: 
##############################################

# NOTE: don't forget to set your library path!
library(ggplot2)
library(reshape2)

#########################################################################
# We first consider the situation where we start with the contingency
# table and not the underlying raw data
#########################################################################

## Manually construct contingency table from pg 7 of the notes, using
## cbind (column bind)
pesticides <- cbind(Present = c(29,19485), "Not Present" = c(98, 7086))
rownames(pesticides) <- c("Organic", "Conventional")
pesticides

## confirm row and column sums ##
colSums(pesticides)
rowSums(pesticides)

## to calculate conditional proportions, use prop.table with
##      margin = 1 to condition on rows
##      margin = 2 to condition on columns
##      margin = NULL (default) to condition on total number of observations

## we use margin = 1 to condition on row (pesticide status -- organic vs. conventional)
pesticides.conditional <- prop.table(pesticides, margin = 1)

# to use ggplot, we need a data.frame with 1 column for the explanatory variable
# (pesticide type), another column for the response variable (presence), and
# another column for the conditional proportion (value) 
# this is accomplished in the two steps below

#convert the table into a data.frame 
d <- data.frame(type = rownames(pesticides.conditional), pesticides.conditional)

# 'melt' the table into one with a new column for the response variable
# and another column for the corresponding value
m <- melt(d, "type", variable.name = "presence")

## display a stacked bar chart, based on 'fill' argument
ggplot(m) + geom_bar(aes(type, value, fill = presence), stat="identity") +
            labs(y = "Proportion", fill = "Pesticide status", 
                 title = "Distribution of pesticide status by food type") +
            theme_classic()


## display a side-by-side barcharts, by changing position argument 
## to 'dodge' in geom_bar
ggplot(m) + geom_bar(aes(type, value, fill = presence), stat="identity", 
                     position = "dodge") +
  labs(y = "Proportion", fill = "Pesticide status", 
       title = "Distribution of pesticide status by food type") +
  theme_classic()


#################################################################
### example of no relationship; remember we must compare 
### conditional proportions between each EXPLANATORY variable and 
### not each response variable
#################################################################

# p2 is contingency table with conditional proportions
p2 <- cbind(Present = c(0.6,0.62), "Not Present" = c(0.4, 0.38))
rownames(p2) <- c("Organic", "Conventional")
p2

# create the 'melted' data.frame
d <- data.frame(type = rownames(p2), p2)
m <- melt(d, "type", variable.name = "presence")

## display a stacked bar chart, based on 'fill' argument
ggplot(m) + geom_bar(aes(type, value, fill = presence), stat="identity") +
  labs(y = "Proportion", fill = "Pesticide status", 
       title = "Distribution of pesticide status by food type\n(no association)") +
  theme_classic()


#########################################################################
# We next consider the situation where we are working with raw data
#########################################################################

## import sample survey data from previous class ##
survey <- read.delim("http://pastebin.com/raw/QDSga7qF")

## is there a relationship between cat/dog person and gaming preference
## create a contingency table, where first vector gives rows, 2nd gives columns
t <- table(survey$CatOrDog, survey$Gaming)
t.conditional <- prop.table(t, margin = 1)

## in ggplot, pass aes(explanatory, fill = response) to the geom_bar layer to 
## look at the explanatory variable (x) and corresponding values of the response variable
## By default, this shows frequencies, which is not what we want (see next example)
ggplot(survey) + geom_bar(aes(CatOrDog, fill=Gaming)) +
                 labs(x = "", y = "Frequency", title = "Gaming preference by person")

## we want conditional proportions, so set position to "fill" so that each bar 
## is scaled to total 100% (i.e., the bars corresond to the conditional 
## proportions for each explanatory variable)
ggplot(survey) + geom_bar(aes(CatOrDog, fill=Gaming), position = "fill") +
                labs(x = "", y = "Relative frequency", title = "Gaming preference by person")


