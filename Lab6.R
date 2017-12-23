#LAb 6
#Jake Cordelli
library(ggplot2)
library(dplyr)

                                        #1
#a
total<- 498+437
p.hat <- 498/total
p <- .5
mu <- p
sd <- sqrt(p*(1-p)/total)

#b
df.phat <- data.frame(x = seq(mu-4*sd, mu+4*sd, length.out = 100)) %>%
  mutate(y = dnorm(x, mean = mu, sd = sd))

plot.phat <- ggplot(df.phat) + geom_line(aes(x, y)) +
  theme_classic() + ggtitle("p.hat ~ N(0.5, sqrt(p*(1-p)/935)") +
  labs(x = "p.hat", y = "density") +
  geom_vline(xintercept = p.hat) + 
  geom_text(aes(x=p.hat-.04, y=6), label = "observed p.hat")
plot.phat

#c
Z <- (p.hat - mu) / (sd)
x <- seq(-4,4, length.out = 100)

df.z <- data.frame(x = seq(-4, 4, length.out = 100)) %>%
  mutate(y = dnorm(x))

plot.z <- ggplot(df.z) + geom_line(aes(x, y)) +
  theme_classic() + ggtitle("Z ~ N(0,1)") +
  labs(x = "Z", y = "density") +
  geom_vline(xintercept = Z) +
  geom_text(aes(x=Z-.8, y=.25), label = "observed Z")
plot.z

#d
p.value = 2*pnorm(-abs(Z))

a <- prop.test(498,935, p = 0.5, correct = FALSE)
b <- a$statistic


c <- prop.test(498,935, p = 0.5, correct =TRUE)
d <- c$p.value

#e
#Because the p-value 0.04973806 < 0.5 it demonstrates that 
#there is not sufficient evidence that Americans prioritize
#"strengthening law and order" over "reducing bias against minorities and therefore, 
#we reject the null hypothesis.

#f
#A type 1 error means rejecting the null hypothesis even though it is actually true. 
#In the context of this problem, it would mean incorrectly deciding
#the priority of Americans in regards to "strengthening law and order"  
#and "reducing bias against minorities. In this case, incorrectly declaring that Americans
#prioritize "reducing bias against minorities" opposed to "strengthening law and order"




                                        #2
#a
n <- 50
p <- (1/6)*50

#Null hypothesis: p = (1/6)*50 
#This refers to the chances of a person being able to accurately predict a dice roll

#Alt hypothesis: the psychic predicts the average roll of a dice
#more than the average person

#b
a <- prop.test(12,50, p = 8/50, correct = FALSE)
b <- a$statistic

#c
c <- prop.test(12,50, p = 8/50, correct =TRUE)
d <- c$p.value

#d
#Because the p-value 0.1769665 > 0.5 it demonstrates that 
#there is not sufficient evidence to support the null hypothesis.
#Therefor, we must reject it and assume the alternative, 
#which in this case, the psychic is legit. 

#e
#a type 2 error means not rejecting the null hypothisis even though 
#the alternative hypothisis was true. In the context of this problem, 
#it would be declaring that p=(1/6)*50.




                                        #3
#a
#Null hypothesis: p = .36
#This refers to the average amount of people who moslty pay with cash only

#Alt hypothesis: 50% of people make all there purchases with cash.
#more than the average person

#b
total <- 1024
x <- 0.36 * total

a <- prop.test(246,total, p = 0.36, correct = FALSE)
b <- a$statistic

#c
c <- prop.test(246,total, p = 0.36, correct =TRUE)
d <- c$p.value

#d
#the p value tells us that we cant reject the null hypothesis, 
#when in fact the alternative hypothesis was true

                         

               
                                        #4
#a
p.valuex <- 2*pnorm(-abs(3.32))
#Because the p-value 0.0009001745 < 0.05 we accept the null hypothesis
#and reject the alternative hypothesis

#b
p.valuey <- 2*pnorm(-abs(-1.3))
#Because the p-value 0.193601 < 0.05 reject the null hypothesis 
#and accept the alternative hypothesis

#c
p.valuez <- 2*pnorm(-abs(-2.02))
#Because the p-value 0.04338339 < 0.05 we accept the null hypothesis
#and reject the alternative hypothesis



                                        #5

#a
#Null hypothesis: Paspirin - Pplacebo  = 0
#Alt hypothesis: Paspirin - Pplacebo != 0

#where paspirin is the probability an individual receiving aspirin will die    
#from cancer, and pplacebo is the probability an individual receiving the
#placebo will die from cancer

#b
prop1 <- 0.03008236
prop2 <- 0.02329890
m1=11535
m2=14035
total=m1+m2
p<-(((m1*prop1) + (m2*prop2))/total)

#c
sd <- sqrt(p*(1-p)*((1/m1)+(1/m2)))

#d
sampleps <- prop.test(x = c(347,327), n = c(m1,m2))

#e
z  <- (prop1 - prop2) / sd

#f
p.value <- 2*pnorm(-abs(z))

#g
#Because the p-value 0.04338339 < 0.05 we accept the null hypothesis
#and reject the alternative hypothesis
#In the context of this problem, this means that there is not a relationship betwen
#aspirin and preventing the rate of death. 

#h
#A type 1 error means rejecting the null hypothesis even though it is actually true. 
#In the context of this problem, declaring that aspirin could reduce the risk of cancer. 

#i
##a type 2 error means not rejecting the null hypothisis even though 
#the alternative hypothisis was true. In the context of this problem, 
#declaring that there is a relationship between a reduced risk of dying
#and using aspirin.
