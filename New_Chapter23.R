#Exercise 1

baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "lung_tumor_expression.csv"
f <- file.path(baseurl, "data", "logistic", filename)
d <- read.csv(f)

head(d)

# boxplots
library(tidyverse)
par(mfrow=c(2,1), las=2)

p1 <- ggplot(data = d, mapping = aes(x = tissue, y = ATP1A2)) +
  geom_boxplot()

p2 <- ggplot(data = d, mapping = aes(x = tissue, y = KDELR2)) +
  geom_boxplot()

require(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# Use gene with best discrimination to do logistic regression
log.fit <- glm(tissue ~ KDELR2,
               data = d,
               family = 'binomial')

# Make confusion matrix
tissues <- levels(d$tissue)

conf_matrix <- data.frame(
  truth = d$tissue,
  prediction = factor(ifelse(predict(log.fit) > 0,
                             tissues[2],
                             tissues[1]
                             ))
)

table(conf_matrix)


# Give fraction of correct predictions
correctfraction <- sum(conf_matrix$truth == conf_matrix$prediction) / dim(conf_matrix)[1]
correctfraction


# Improve the model by adding the other gene
log.fit2 <- glm(tissue ~ KDELR2 + ATP1A2, data = d, family = 'binomial')
log.fit3 <- glm(tissue ~ KDELR2 * ATP1A2, data = d, family = 'binomial')

confusion2 <- data.frame(truth = d$tissue, prediction = factor(ifelse(predict(log.fit2) > 0, tissues[2], tissues[1])))
confusion3 <- data.frame(truth = d$tissue, prediction = factor(ifelse(predict(log.fit3) > 0, tissues[2], tissues[1])))

correctfraction2 <- sum(confusion2$truth == confusion2$prediction) / dim(confusion2)[1]
correctfraction3 <- sum(confusion3$truth == confusion3$prediction) / dim(confusion3)[1]




# Exercise 2: Predicting iris species
set.seed(42)
n <- trunc(0.8 * dim(iris)[1])
trainset <- sample(x = rownames(iris), size = n)
testset <- rownames(iris)[!rownames(iris) %in% trainset]

trainsamples <- iris[trainset,]
testsamples <- iris[testset,]


# Calculating priors (= expected fraction of 3 species)
prior <- tapply(X = rep(x = 1/n,
                        times = n),
                INDEX = trainsamples$Species,
                FUN = sum)

# Calculate means and std. dev
nb <- lapply(
  apply(trainsamples[1:4], 2, function(x){
    tapply(x, trainsamples[[5]], function(x){
      c(mean(x),sd(x))})}
  ),
  function(x){do.call(rbind,x)}
)
