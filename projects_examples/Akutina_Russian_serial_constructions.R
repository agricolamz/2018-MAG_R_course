y <- read.csv("~/Desktop/2verbCUTimp.csv", header = TRUE)
y

library(ggplot2)
library(tidyverse)
library(party)
library(lattice)

#1.correlation between punctuation and mood
y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

a <- ggplot(data = y, aes(x = V2, fill = IMP)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and mood") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
a

#2.correlation between punctuation and semantic type
y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

b <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
b

#3.correlation between punctuation and construction
c <- ggplot(data = y, aes(x = V2, fill = construction)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and construction") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c + theme_classic()

#4.correlation between punctuation and different features (GLM)
fit <- glm(V2~EL, data = y, family = "binomial")
summary(fit)

fit <- glm(V2~type + IMP, data = y, family = "binomial")
summary(fit)





#5.type+IMP (ctree) 

c=read.table("~/Desktop/2cutmp.csv", sep=",", header=TRUE)
head(c)

cf <- cforest(V2 ~ type+IMP, data = c, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf)

#importance      type         IMP    0.052954545     0.005227273

#6.correlation between punctuation and EL
y <- read.csv("~/Desktop/2verbCUTel.csv", header = TRUE)
p <- ggplot(data = y, aes(x = V2, fill = EL)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and construction") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

p

#7.RNC
y <- read.csv("~/Desktop/nrc.2verbconstr.csv", header = TRUE)
d <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

d

#8.triplets
y <- read.csv("~/Desktop/tripletsnotCUT.csv", header = TRUE)
c <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c

#9.triplets
y <- read.csv("~/Desktop/tripletsnotCUT.csv", header = TRUE)
c <- ggplot(data = y, aes(x = V3, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V3") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c

y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

mylm <- lm(as.numeric(V2)~as.numeric(type), data = y) 
plot(mylm)
mylm

#10.predict+vis for 2 verb constructions (type+IMP)

y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

mylogit <- glm(V2 ~ type + IMP, data = y, family = "binomial")
summary(mylogit)

mynewdata <- read.csv("~/Desktop/ammmmm.csv", header = TRUE)
commaetc <- predict(mylogit, newdata = mynewdata, type = "response", se.fit = TRUE)
commaetc
mynewdata$comma <- commaetc$fit
mynewdata$se.comma <- commaetc$se.fit
mynewdata$labels <- paste(mynewdata$type, mynewdata$IMP)
mynewdata

ggplot(mynewdata, aes(x = reorder(labels, comma), y = comma,group = 1)) + 
  geom_errorbar(aes(ymin=comma-se.comma/2,ymax=comma+se.comma/2), color = 'pink',width=.1, size = 1) +
  geom_point(aes(reorder(labels, comma),comma),size = 2) +
  #geom_point(aes(reorder(labels, comma),(comma-se.comma/2)),color = 'pink', size = 2) +
  #geom_point(aes(reorder(labels, comma),(comma+se.comma/2)),color = 'pink', size = 2) +
  ggtitle("Correlation between punctuation and type+form") +
  xlab("features")+ylab('probability of comma') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))




