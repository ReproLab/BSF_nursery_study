library(car)
library(dplyr)
library(car)
#The first part below gives the results of Table 1 and Table S1
#ANOVA for post-nursery rearing 1 after NE1
aa<-read.csv(file.choose()) #select file "Nursery_1"
#1.for larvae rearing from day-5 neonates
a<-filter(aa,nt=="day5")
attach(a)
#####1.1 for final prepupal weight#####
qqPlot(lm(pw ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pw ~ meal, data = a) 

aggregate(pw, by = list(meal), FUN = mean)
aggregate(pw, by = list(meal), FUN = sd)
fit <- aov(pw ~ meal)
summary(fit) 
######1.2 for survival rate######
qqPlot(lm(sr ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(sr ~ meal, data = a) 

aggregate(sr, by = list(meal), FUN = mean)
aggregate(sr, by = list(meal), FUN = sd)
fit <- aov(sr ~ meal)
summary(fit) 
detach(a)
#2. for larvae rearing from day-10 neonates
a<-filter(aa,nt=="day10")
#####2.1 for final prepual weight#####
qqPlot(lm(pw ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pw ~ meal, data = a) 

aggregate(pw, by = list(meal), FUN = mean)
aggregate(pw, by = list(meal), FUN = sd)
fit <- aov(pw ~ meal)
summary(fit) 
TukeyHSD(fit)
out <- LSD.test(fit,"meal",p.adj="bonferroni") 
out
######2.2 for survival rate######
qqPlot(lm(sr ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(sr ~ meal, data = a) 

aggregate(sr, by = list(meal), FUN = mean)
aggregate(sr, by = list(meal), FUN = sd)
fit <- aov(sr ~ meal)
summary(fit) 
detach(a)

#ANOVA for post-nursery rearing 2 after NE2
a<-read.csv(file.choose()) ##select file "Nursery_2"
head(a)
#####1 for final prepupal weight#####
qqPlot(lm(pw ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pw ~ meal, data = a) 
attach(a)
aggregate(pw, by = list(meal), FUN = mean)
aggregate(pw, by = list(meal), FUN = sd)
fit <- aov(pw ~ meal)
summary(fit) 
######1.2 for survival rate######
qqPlot(lm(sr ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(sr ~ meal, data = a) 

aggregate(sr, by = list(meal), FUN = mean)
aggregate(sr, by = list(meal), FUN = sd)
fit <- aov(sr ~ meal)
summary(fit) 
detach(a)

#The second part below gives the results of Table 4

cc<-read.csv(file.choose()) ##select file "NE1&2_larvalweight"
head(cc)
#1. Nursery experiment 1, individual larval weight (lw) in post-nursery rearing from day 2-8
c_1<-filter(cc,test=="NE1")
attach(c_1)
#####NE1* day2 * larval weight#####
qqPlot(lm(lw_2 ~ meal, data = c_1), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_2 ~ meal, data = c_1) 

fit <- aov(lw_2 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day4 * larval weight#####
qqPlot(lm(lw_4 ~ meal, data = c_1), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_4 ~ meal, data = c_1) 

fit <- aov(lw_4 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day6 * larval weight#####
qqPlot(lm(lw_6 ~ meal, data = c_1), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_6 ~ meal, data = c_1) 

fit <- aov(lw_6 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day8 * larval weight#####
qqPlot(lm(lw_8 ~ meal, data = c_1), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_8 ~ meal, data = c_1) 

fit <- aov(lw_8 ~ meal)
summary(fit) ##extract F value and P value for table 4
detach(c_1)
#2. Nursery experiment 2, individual larval weight (lw) in post-nursery rearing from day 2-8
c_2<-filter(cc,test=="NE2")
attach(c_2)
#####NE1* day2 * larval weight#####
qqPlot(lm(lw_2 ~ meal, data = c_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_2 ~ meal, data = c_2) 

fit <- aov(lw_2 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day4 * larval weight#####
qqPlot(lm(lw_4 ~ meal, data = c_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_4 ~ meal, data = c_2) 

fit <- aov(lw_4 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day6 * larval weight#####
qqPlot(lm(lw_6 ~ meal, data = c_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_6 ~ meal, data = c_2) 

fit <- aov(lw_6 ~ meal)
summary(fit) ##extract F value and P value for table 4
#####NE1* day8 * larval weight#####
qqPlot(lm(lw_8 ~ meal, data = c_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(lw_8 ~ meal, data = c_2) 

fit <- aov(lw_8 ~ meal)
summary(fit) ##extract F value and P value for table 4
detach(c_2)
