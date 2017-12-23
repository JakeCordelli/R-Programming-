##############################################################
# Name: Jake Cordelli
# CSC-315
# Lab #9: Limma, heatmaps, and analyzing processed GEO data
#############################################################

##########################################################################
# Add R code to the script below and create a Notebook which to complete
# the steps and explicitly answer the following questions
##########################################################################

library(limma)
library(ggplot2)
library(dplyr)

##################################################################################
# 1.The code below reads in the survey data and performs a 
#   2-sample t-test to evaluate whether there is a statistically
#   significant difference in alcohol consumption between 'Cat' vs. 'Dog'
#   people. Based on the code below, (a) find the p-value and state 
#   your conclusion regarding the null hypothesis of H0: mu_cat - mu_dog = 0;
#   and (b) calculate the difference in mean Alcohol consumption between
#   groups, using the formula: 
#     mean consumption of dog people - mean consumption of cat people
##################################################################################
survey <- read.csv("https://gdancik.github.io/CSC-315/data/datasets/survey_fall-2017.csv")
s <- split(survey$Alcohol, survey$CatOrDogPerson)
res <- t.test(s$Cat, s$Dog, var.equal = TRUE)
p <- res$p.value

# a) Because the p-value 0.2780401 > 0.05, we must acept the null hyptothesis because we fail to reject it
# In the context of this problem, this means that there is not a major difference in alcohol consumption
# between a catPerson and a dogPerson. 
catPerson <- mean(s[[1]])
dogPerson <- mean(s[[2]])

difference <- dogPerson - catPerson
difference



##################################################################################
# 2.Fit a linear model that predicts Alchohol consumption based on 
#   whether an individual is a cat or a dog person. You should use
#   the treatment contrast where 'cat person' is the reference (x = 0) and 
#   'dog person' is the treatment (x = +1)
#    
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.
# (b) Find and interpret the slope in the context of this problem
# (c) What is the p-value for the hypothesis test that there is a
#     significant difference in Alcohol consumption between the two groups?
#     (show this result in R, based on the linear model)
##################################################################################
petType <- as.integer(survey$CatOrDogPerson == "Dog")
df <- mutate(survey, petType = as.integer(CatOrDogPerson == "Dog"))

ggplot(df, aes(petType, Alcohol)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_classic() + 
  xlab("Pet Type (0 = Cat, 1 = Dog)") + ylab("Alcohol Consumption per week") +
  ggtitle("Drinking Status of cat(0) and dog(1) people")

fit <- lm(survey$Alcohol ~ petType)
summary(fit)

# a) The y-intercept is .3750, meaning that a catPerson is most liekly to drink .3750 times per week

# b) The slope is .8036, meaning that the average dogPerson drinks roughly %80.36 
# more alcohol than the average catPerson

# c) The p-value is .278 > 0.05, meaning we accept the null hypothesis because we fail to reject it. 
# In the context of this problem, this means that there is not a major difference between the alcohol 
# consumption of a dogPerson vs a catPerson. 


###############################################################
# 3. Get the processed data for GSE19143 and pull out the 
#    expression data and phenotype data. Note that this
#    dataset contains gene expression samples from children
#    with Acute Lymphoblastic Leukemia (ALL), a cancer of
#    the bone marrow. Tumor samples were treated with
#    the anti-inflammatory drug prednisolone, and determined 
#    to be either sensitive (responsive) or resistant 
#    (non-responsive) to this drug. 
###############################################################
library(GEOquery)
GSE19143 <- getGEO("GSE19143")

# (a) How many samples had their gene expression values profiled?
GSE19143.expr <- exprs(GSE19143[[1]])
GSE19143.expr <- log2(GSE19143.expr)
ncol(GSE19143.expr)

# (b) How many probes are on the array?
nrow(GSE19143.expr)

#####################################################################
# 4.How many individuals are resistant to prednisolone and
# how many are sensitive? 
#####################################################################
GSE19143.p <- pData(GSE19143[[1]])

sampleNames <- as.character(GSE19143.p$title)
s <- strsplit(sampleNames, " ")

get.fourth <- function(x) {
  return(x[[4]])
}

groups <- sapply(s, get.fourth)

GSE19143.p <- mutate(GSE19143.p, status = groups)

resist <- filter(GSE19143.p, status == "resistant")
nrow(resist)

sensitive <- filter(GSE19143.p, status == "sensitive")
nrow(sensitive)

#####################################################################
# 5. Find the top differentially expressed probes, with a FDR of 10%,
# between individuals that are resistant vs. sensitive to prednisolone.
# Note: there should be 16 probes total. How many of these probes 
# are up-regulated (i.e., have higher expression) in resistant 
# individuals and how many are down-regulated (i.e., have lower 
# expression) in resistant individuals. 
#####################################################################
diffex <- as.character(GSE19143.p$status)
levels(diffex) <- c("Sensitive", "Resistant")
design <- model.matrix(~0+diffex)
colnames(design) <- c("Sensitive", "Resistant")

fit <- lmFit(GSE19143.expr, design)

contrast.matrix <- makeContrasts(Sensitive - Resistant,levels=design)

fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
tt.10 <- topTable(fit2,sort.by = "p", p.value = 0.20, number = nrow(GSE19143.expr))
nrow(tt.10)

########################################################################
# 6. Construct a heatmap of these top 16 probes, with individuals 
# color-coded by response to prednisolone (with green=sensitive and 
# red = resistant). (Note: if you are unable to complete question 5), 
# you may do this with the first 16 probes in the expression matrix).
########################################################################
m <- match(rownames(tt.10), rownames(GSE19143.expr))
X <- GSE19143.expr[m,]

col.heat <- colorRampPalette(c("yellow", "blue"))(200)
col.status <- as.integer(as.factor(diffex))
col.status <- c("green", "red")[col.status]

heatmap(X, ColSideColors = col.status, col = col.heat)

########################################################################
# 7. If you answered question 5 correctly, the SECOND hit 
# should be for the probe 209374_s_at. Show that this probe
# corresponds to the gene IGHM, by first downloading the 
# correct platform data from GEO, and then finding the gene
# associated with this probe. 
#######################################################################
platform <- annotation(GSE19143[[1]])
pl <- getGEO(platform)
pl <- Table(pl)

probe <- rownames(tt.10)[2]
m <- match(probe, pl$ID)
pl$`Gene Symbol`[m]

#####################################################################
# 8. How many probes are there for the gene IGHM on the platform
# in this study? Note: you must search for this gene using the
# regular expressions covered in the GEO-and-limma.R script. Your 
# code must also output the number of probes. 
####################################################################
g <- grep("^IGHM$| IGHM |^IGHM | IGHM$", pl$`Gene Symbol`)
length(pl$ID[g])

########################################################################
# Final Notes: the heatmap in question 6 provides a candidate list
# of probes associated with prednisolone response in children with 
# leukemia. Although much additional work and testing needs to be done, 
# this kind of gene signature could ultimately be used to determine whether a 
# child with leukemia would benefit from prednisolone treatment, or 
# whether an alternative treatment might be more effective.

# The IGHM finding is also interesting. IGHM is a gene that codes
# for an antibody protein involved in the immune reponse; the 
# fact that this gene is differentially expressed beween responders and 
# non-respnoders suggests that a patient's immune system may play a
# role in how they respond to prednisolone)
########################################################################
