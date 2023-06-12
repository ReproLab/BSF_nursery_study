library(car)
library(multcomp)
library(gplots)
library(agricolae) 
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(jcolors)
library(FSA)
library(ggbiplot)#may need to install_github("vqv/ggbiplot")

###figures for 5/10-day neonate weight in NE1#####
#bars
data_a<-read.csv(file.choose()) ###select file"nursery_1.csv"
data_a
c<-filter(data_a,nt=="day5")
c$nw
attach(c)
nw<-unlist(nw)
qqPlot(lm(nw ~ meal, data.frame = c), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(c$nw ~ meal, data = c) 
fit_1 <- aov(c$nw ~ meal)
TukeyHSD(fit_1)
out_1 <- LSD.test(fit_1,"meal",p.adj="bonferroni") 
aa_1 = out_1$groups
sd_1 = as.data.frame(tapply(c$nw,c$meal,sd,na.rm=TRUE))
se_1 = sd_1/sqrt(3)
all_1 = merge(aa_1,se_1, by="row.names",all=F)
names(all_1)<-c("meal","mean","sig","se")
all_1$nt<-c("day5", "day5","day5",
            "day5","day5")
detach(c)

cc<-filter(data_a,nt=="day10")
attach(cc)
nw<-unlist(nw)
qqPlot(lm(cc$nw ~ meal, data.frame = cc), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(cc$nw ~ meal, data = cc) 
fit_2 <- aov(cc$nw ~ meal)
TukeyHSD(fit_2)
out_2 <- LSD.test(fit_2,"meal",p.adj="bonferroni") 
aa_2 = out_2$groups
sd_2 = as.data.frame(tapply(cc$nw,cc$meal,sd,na.rm=TRUE))
se_2 = sd_2/sqrt(3)
all_2 = merge(aa_2,se_2, by="row.names",all=F)
names(all_2)<-c("meal","mean","sig","se")
all_2$nt<-c("day10", "day10","day10",
            "day10","day10")
detach(cc)
all<-rbind(all_1, all_2)
all
ggplot(all, aes(x = reorder(meal,mean),y=mean)) + 
  geom_bar(stat = "identity", position = "dodge",colour="black",width=.7,size=.5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,size=1)+
  geom_line(size=.8)+theme_bw()+
  theme(
    strip.background = element_rect(
      color = "white", fill = "white"),
    panel.grid = element_blank())+
  theme(
    axis.title = element_text(size =17, face="bold"),
    axis.text = element_text(vjust=1,size=13,face="bold",colour = "black"),
    axis.ticks = element_line(size = rel(2.5)),
    axis.ticks.length = unit(0.5, "cm"),
    panel.border = element_rect(colour = "black",size=2.5,fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+theme(strip.text = element_text(color="black",size = 12,face = "bold"))+
  theme(axis.ticks=element_line(color="black",size=1))+
  theme(axis.text=element_text(color="black",size=10,face = "bold"))+
  labs(
    x="Meal",
    y="Individual Weight of Neonates (mg)"
  )+
  facet_grid(.~nt,space="fixed",scales = "free")+
  geom_text(
    aes(label=sig,y=mean+se,x=meal), hjust=-0.3,
    size = 5)+
  coord_flip()
#########end##########

###figures for 5-day neonate weight in NE2#####
##bars
data_b<-read.csv(file.choose()) ###select file"nursery_2.csv"
data_b
attach(data_b)
qqPlot(lm(nw_5 ~ meal, data.frame = data_b), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(nw_5 ~ meal, data = data_b) 
##p=0.02<0.05, conduct kruskal-wallis test and Dunn test instead
kruskal.test(nw_5 ~ meal, data = data_b)
data_b$meal <- as.factor(data_b$meal)
dunn_result <- dunnTest(data_b$nw_5, data_b$meal, method = "bonferroni")
print(dunn_result)
out <- data.frame(
  meal = c("CF", "F1","F1_pH", "F2",  "F3", "F3_pH"),
  groups = c("a", "c", "bc", "abc", "ab", "ab")
) ###create "letter labeling" according to the result of dunn test (P.unadj)
row.names(out) <- out$meal
mean = as.data.frame(tapply(nw_5,data_b$meal,mean,na.rm=TRUE))
sd = as.data.frame(tapply(data_b$nw_5,data_b$meal,sd,na.rm=TRUE))
se = sd/sqrt(3)
aaa = cbind(mean,se)
all = merge(out,aaa, by="row.names",all=F)
all <- all[, -1]

names(all)<-c("meal","sig","mean","se")
all
attach(all)
ggplot(all, aes(x = reorder(meal,mean),y=mean)) + 
  geom_bar(stat = "identity", 
           position = "dodge",width=.7,size=.5,colour="black",
           fill=c("CF"="#C82423","F1"="#2878B5", "F1_pH"="#2878B5", 
                  "F2"="#9AC9DB","F3"="#F8AC8C","F3_pH"="#F8AC8C"))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,size=1)+
  geom_line(size=.8)+theme_bw()+
  theme(
    strip.background = element_rect(
      color = "white", fill = "white"),
    panel.grid = element_blank())+
  theme(
    axis.title = element_text(size =17, face="bold"),
    axis.text = element_text(vjust=1,size=13,face="bold",colour = "black"),
    axis.ticks = element_line(size = rel(2.5)),
    axis.ticks.length = unit(0.5, "cm"),
    panel.border = element_rect(colour = "black",size=2.5,fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+theme(strip.text = element_text(color="black",size = 12,face = "bold"))+
  theme(axis.ticks=element_line(color="black",size=1))+
  theme(axis.text=element_text(color="black",size=10,face = "bold"))+
  labs(
    x="Meal",
    y="Individual Weight of 5-day Neonates (mg)"
  )+
  geom_text(
    aes(label=sig,y=mean+se,x=meal), hjust=-0.3,
    size = 5)+
  coord_flip()
#########end##########


###the emergence rate of prepupae###
data_c<-read.csv(file.choose()) ###select file"nursery_2_pp.csv"
data_c
c<-filter(data_c,day=="Day 8")
attach(c)
n<-table(meal)
qqPlot(lm(pp ~ meal, data.frame = c), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pp ~ meal, data = c) 
fit <- aov(pp ~ meal)
summary(fit)####output the P value for day 8
mean = as.data.frame(tapply(c$pp,c$meal,mean,na.rm=TRUE))
sd = as.data.frame(tapply(c$pp,c$meal,sd,na.rm=TRUE))
se = sd/sqrt(3)####calculate SE value for error bars
pp_8 = cbind(mean,se)
names(pp_8)<-c("pp","se")
pp_8=cbind(meal=row.names(pp_8), pp_8)
pp_8$day<-c("Day 8", "Day 8","Day 8", "Day 8",
            "Day 8", "Day 8")
pp_8$com<-c("a", "a","b", "c",
            "c", "cf")
pp_8
detach(c)

c<-filter(data_c,day=="Day 9")
attach(c)
n<-table(meal)
qqPlot(lm(pp ~ meal, data.frame = c), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pp ~ meal, data = c) 
fit <- aov(pp ~ meal)
summary(fit)####output the P value for day 9
mean = as.data.frame(tapply(c$pp,c$meal,mean,na.rm=TRUE))
sd = as.data.frame(tapply(c$pp,c$meal,sd,na.rm=TRUE))
se = sd/sqrt(3)####calculate SE value for error bars
pp_9 = cbind(mean,se)
names(pp_9)<-c("pp","se")
pp_9=cbind(meal=row.names(pp_9), pp_9)
pp_9$day<-c("Day 9", "Day 9","Day 9", "Day 9",
            "Day 9", "Day 9")
pp_9$com<-c("a", "a","b", "c",
            "c", "cf")
pp_9
detach(c)

c<-filter(data_c,day=="Day 10")
attach(c)
n<-table(meal)
qqPlot(lm(pp ~ meal, data.frame = c), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(pp ~ meal, data = c) 
fit <- aov(pp ~ meal)
summary(fit)####output the P value for day 10
mean = as.data.frame(tapply(c$pp,c$meal,mean,na.rm=TRUE))
sd = as.data.frame(tapply(c$pp,c$meal,sd,na.rm=TRUE))
se = sd/sqrt(3)####calculate SE value for error bars
pp_10 = cbind(mean,se)
names(pp_10)<-c("pp","se")
pp_10=cbind(meal=row.names(pp_10), pp_10)
pp_10$day<-c("Day 10", "Day 10","Day 10", "Day 10",
            "Day 10", "Day 10")
pp_10$com<-c("a", "a","b", "c",
            "c", "cf")
pp_10
detach(c)

pplot<-rbind(pp_8, pp_9,pp_10)

pplot$day_f = factor(pplot$day, levels=c('Day 8','Day 9','Day 10'))
ggplot(pplot, aes(x = reorder(meal,pp),y=pp,fill=com)) + 
  geom_bar(stat = "identity", 
           position = "dodge",
           width=.7,size=.5,colour="black")+
  geom_errorbar(aes(ymin=pp-se, ymax=pp+se), width=.1,size=1)+
  geom_line(size=.8)+theme_bw()+
  labs(
    x="Meal",
    y="The Emergence Rate of Prepupae"
  )+ 
  facet_grid(.~day_f,space="fixed",scales = "fixed")+
  coord_flip()+
  scale_fill_manual(values= c("a"="#2878B5", "b"="#9AC9DB","c"="#F8AC8C",
                              "cf"="#C82423"))+
  theme_bw()+
  theme(
    strip.background = element_rect(
      color = "white", fill = "white"),
    panel.grid = element_blank())+
  theme(
    axis.title = element_text(size =17, face="bold"),
    axis.text = element_text(vjust=1,size=13,face="bold",colour = "black"),
    axis.ticks = element_line(size = rel(2.5)),
    axis.ticks.length = unit(0.5, "cm"),
    panel.border = element_rect(colour = "black",size=2.5,fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )+theme(strip.text = element_text(color="black",size = 12,face = "bold"))+
  theme(axis.ticks=element_line(color="black",size=1))+
  theme(axis.text=element_text(color="black",size=10,face = "bold"))
#########end##########

###PCA for meal nutrients###
pca<-read.csv(file.choose(),head=T)###select file"nursery_1 meals for PCA"
head(pca)

pca_meal <- prcomp(pca[,c(3:6)],center = TRUE,scale= TRUE)
pca_meal
summary(pca_meal)

ggbiplot(pca_meal, ellipse = TRUE, obs.scale = 1, var.scale = 1,varname.size = 4.5,
         labels = NULL, shape = pca$type, fill = pca$meal) +
  geom_point(aes(colour = pca$meal,shape = pca$type),size=4) +
  scale_shape_manual(values = c(16, 17)) +
  theme(axis.title.x = element_text(face = "bold", size = 14.5)) +
  theme(axis.title.y = element_text(face = "bold", size = 14.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white')) +
  theme(strip.text = element_text(color = "black", size = 12, face = "bold")) +
  theme(axis.ticks = element_line(color = "black", size = 1))+
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key = element_blank()) + 
  theme(legend.background = element_blank())+
  labs(colour = "meal", shape = "type")+
  theme(legend.spacing.y = unit(0.01, "cm"))+
  theme(axis.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) 
#########end#######

