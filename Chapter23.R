# Read data (23.1.)
baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "bodymetrics.tab"

url <- file.path(baseurl, 'data', 'linearmodels', filename)
bodymetrics <- read.table(url, sep = '\t', header = T)
head(bodymetrics)

# Make a histogram of the data
library(tidyverse)
p <- ggplot(data = bodymetrics,
       mapping = aes(x = height,
                     fill = gender)) +
  geom_histogram(position = 'identity',
                 alpha = 0.7)

# logistic equation p(x) = 1/(1+e^(-f(x))), where
# f(x) = logit function = ln(p/(1-p))

# Generalized Linear Modeling (GLM)
# - Response variable (y) does not have to be norm.-distributed
# - Linear function of the predictor vars is transformed through a link function (logit)

#Logistic regression model
log.fit <- glm(gender ~ height,
               data = bodymetrics,
               family = 'binomial')


log.fit2 <- glm(gender ~ height + weight,
                data = bodymetrics,
                family = 'binomial')



### EXERCISE 1 ###
# A
filename <- "lung_tumor_expression.csv"
url <- file.path(baseurl, 'data', 'logistic', filename)
genes <- read.csv(url)
head(genes)

# B
# 2 genes: ATP1A2 & KDELR2
par(mfrow=c(1,2), las = 2)

boxplot(ATP1A2 ~ tissue,
        data = genes)
boxplot(KDELR2 ~ tissue,
        data = genes)

# C
# KDELR2 has the best discrimination
# Make a logistic regression
log.fit <- glm(tissue ~ KDELR2,
               data = genes,
               family = 'binomial')

# tissues = 'Lung tumor', 'normal lung'
tissues <- levels(genes$tissue)

# Make a confusion matrix
confusion <- data.frame(
  truth = genes$tissue,
  prediction = factor(ifelse(predict(log.fit) > 0,
                             tissues[2],
                             tissues[1]))
)

table(confusion)

correctfraction <- sum(confusion$truth == confusion$prediction) / dim(confusion)[1]


# D
log.fit2 <- glm(tissue ~ KDELR2 + ATP1A2,
               data = genes,
               family = 'binomial')


confusion2 <- data.frame(
  truth = genes$tissue,
  prediction = factor(ifelse(predict(log.fit2) > 0,
                             tissues[2],
                             tissues[1]))
)

table(confusion2)

correctfraction
correctfraction2 <- sum(confusion2$truth == confusion2$prediction) / dim(confusion2)[1]
correctfraction2
# -> Does not improve!


## 23.2

# Bays theorem:
# PR(G\P = Pr(G) * Pr(P\G) / Pr(P))

# MAP rule:
# value that maximizes Pr(G\P)

# Chain rule:
# Pr(P,S,G) = Pr(P\S, G) * Pr(S\G) * Pr(G)
#           = Pr(P\G) * Pr(S\G) * P(G)
#           = Pr(G\P, S) * Pr(P, S)

# Pr(G\P, S) = (Pr(P\G) * Pr(S\G) * Pr(G)) / Pr(P, S)

# EXAMPLE: iris
# response variable: Species
# predictor variables: Sepal.Length, Septal.Width, Peta.Length, Petal.Width



# Exercise 1
library('caret')
inTrain <- createDataPartition(y = iris$Species, p = .80, list = F)
training <- iris[inTrain, ]
test <- iris[-inTrain, ]

# Exercise 2
# Calculate 1x3 array of priors from test set
prior <- test %>% group_by(Species) %>% summarise(total = n() / nrow(test))

prior2 <- tapply(rep(1/length(training$Species),
                     length(training$Species)),
                 training$Species,
                 sum)


# Exercise 3
#Calculate means and standard deviations of the four predictor variables 
# for each of the species using the training set
# -> matrix of 3 rows (Species) and 2 columns (mean & std. dev)

nb <- lapply(apply(training[1:4], 2, function(x) {
  tapply(x, training[[5]], function(x) {
    c(mean(x), sd(x))
  })
}), function(x) {
  do.call(rbind, x)
})


# Exercise 4
posteriors <- t(apply(test[1:4], 1, function(y) {
  apply(sapply(names(y),
               function(varname) {
                 apply(nb[[varname]], 1, function(x) {
                   dnorm(y[[varname]], x[1], x[2])
                 })
               }), 1, prod)
}))

# Exercise 5
# Make predictions
predictions <- posteriors
pred <- colnames(predictions)[apply(predictions,1,which.max)]
predictions <- cbind(predictions, pred)

# Compare to test set
final_tb <- test
tb_merged <- merge(final_tb, predictions[, "pred"], by = "row.names")
colnames(tb_merged)[7] <- "Prediction"
tb_filt <- tb_merged[, c("Species", "Prediction")]

tb_filt2 <- tb_filt %>% 
  mutate(Accuracy = ifelse(Species == Prediction, TRUE, FALSE))

tb_filt2

## solution from slides
priors <- c(1/3,1/3,1/3)
predictions2 <- colnames(posteriors)[max.col(t(t(posteriors)*priors))]
correct <- sum(predictions2==as.character(test[[5]]))/length(predictions2)



######## Predicting income ##############

# Exercise 7
library(e1071)
library(caret)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
adult <- read.table("adult.data")
column_names <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation",
                  "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week",
                  "native-country", "income")
colnames(adult) <- column_names

trainset <- sample(rownames(adult), size=trunc(0.8*dim(adult)[1]))
testset <- rownames(adult)[!(rownames(adult) %in% trainset)]
trainsamples <- adult[trainset,]
testsamples <- adult[testset,]

model <- naiveBayes(income ~. , data = trainsamples)
prediction <- predict(model, testsamples)

correct <- sum(prediction == as.character(testsamples[[15]])) / length(prediction)

