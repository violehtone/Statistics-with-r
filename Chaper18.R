######################
# Chapter 18
######################

# functions introduced
relevel()
contrasts()
lm()
summary()
anova()
coefficients()

#ANOVA
# one-way ANOVA = 1 predictor variable
# two-way ANOVA = 2 predictor variables


# Exercises 18.2
# transformation efficiency ~ way of preparation
baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "plasmidtransformation.tab"

url <- file.path(baseurl, 'data', 'linearmodels', filename)
d <- read.table(url, sep = '\t', header = T)
head(d)

#relevel data
d$t <- relevel(d$t, 'normal')
d$log.transforms <- log(d$transforms, 10)

# model with plasmidprep as additive / interaction term
model.total.sum <- lm(log.transforms ~ plasmidprep + t*h,
                      data = d)


model.total.interact <- lm(log.transforms ~ plasmidprep * t * h,
                           data = d)

# perform anova analysis
anova(model.total.sum, model.total.interact)

# fit the data
d.s <- subset(d, plasmidprep == 'syngeneic')
model.s.sum <- lm(log.transforms ~ t + h, data = d.s)
model.s.interact <- lm(log.transforms ~ t * h, data = d.s)
anova(model.s.sum, model.s.interact)

# simpler model
model.s.tonly <- lm(log.transforms ~ t, data = d.s)
anova(model.s.tonly, model.s.sum)


### 18.3
library(tidyverse)

#load data
filename <- "bodymetrics.tab"
url <- file.path(baseurl, 'data', 'linearmodels', filename)
bodymetrics <- read.table(url, sep = '\t', header = T)
head(bodymetrics)

#plot data
ggplot(data = bodymetrics, mapping = aes(x = height,
                                         y = weight,
                                         color = gender,
                                         shape = gender)) +
  geom_point(size = 3) +
  stat_smooth(method = lm, se = F, colour = 'blue')
  

onelinemodel <- lm(weight ~ height, data = bodymetrics)
summary(onelinemodel)
twolinemodel <- lm(weight ~ height * gender, data = bodymetrics)

anova(onelinemodel, twolinemodel)

singleslopemodel <- lm(weight ~ height + gender, data =bodymetrics)
summary(singleslopemodel)

anova(singleslopemodel, twolinemodel)

# 18.3.1 exercises
#1
head(bodymetrics)
singleinterceptmodel <- lm(weight ~ height/gender, data =bodymetrics)
anova(singleinterceptmodel, twolinemodel)

#2
ggplot(data = bodymetrics,
       mapping = aes(x = weight,
                     y = age,
                     color = gender)) +
  geom_point()

agemodel <- lm(weight ~ height + age + gender, data = bodymetrics)
summary(agemodel)

anova(singleslopemodel, agemodel)
