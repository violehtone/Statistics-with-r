library(tidyverse)

# logistic regression model
log.model <- glm(Species ~ Sepal.Length,
                 data = iris %>% filter(Species != "setosa"),
                 family = "binomial")

pred <- data.frame(Species = predict(log.model, iris), Sepal.Length = iris$Sepal.Length)

# Plot data
ggplot(data = iris %>% filter(Species != "setosa"),
       mapping = aes(x = Sepal.Length,
                     fill = Species)) +
  geom_histogram(position = 'identity',
                 alpha = 0.7) +
  stat_function(fun = log.model$formula)


# Make a confusion matrix
iris_filt <- iris %>% filter(Species != "setosa")
species <- levels(iris_filt$Species)


confusion <- data.frame(
  truth = iris_filt$Species,
  prediction = factor(ifelse(predict(log.model) > 0,
                                     species[2],
                                     species[1]))
  )

table(confusion)


# Utilize more predictors
full.log.model <- glm(Species ~ Petal.Width + Petal.Length +
                        Sepal.Width + Sepal.Length,
                      family = "binomial",
                      data = iris %>% filter(Species != "setosa"))

full.log_confusion <- data.frame(
  truth = iris_filt$Species,
  prediction = factor(ifelse(predict(full.log.model) > 0,
                             species[2],
                             species[1]))
)

table(full.log_confusion)


# Linear discriminant analysis (LDA)
library(MASS)
lda.model <- lda(Species ~., data = iris)

# Plot results
plot(lda.model, col = as.integer(iris$Species))


# Perform classification based on the LDA model
lda.model.c <- predict(lda.model, newdata = iris[, c(1,2,3,4)])$class

table(iris[,5], lda.model.c)

# Obtain probability distributions over class labels
sample <- sample_n(iris, 5)
predictions <- predict(lda.model, newdata = sample)
bind_cols(sample["Species"], as.data.frame(predictions$posterior))


# Decision tree
library(rpart)
library(rpart.plot)
rtree <- rpart(Species ~ Petal.Length + Petal.Width, iris)
rpart.plot(rtree)


# Regression tree
tree.cars <- rpart(Price ~., car90)
rpart.plot(tree.cars)
