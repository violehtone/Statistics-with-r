# Chapter 21: Example : Linear regression using many predictors


## Theory
# --------------------------------------------------------------------
# L2 norm = penalty term used in Ridge regression
# L1 norm = penalty term used in Lasso regression

# Main difference?
# - In Lasso, many coefficients will become 0
# - In Ridge, coefficients will have low values but > 0

# Problem with Lasso?
# - If features are correlated, the selection of them can vary a lot

# Elastic Net regression - best of both worlds!

# --------------------------------------------------------------------

# Exercises:

# Data set
# - 11 predictor variables
# - Response variable: quality of wine (score of 0-10)

# --------------------------------------------------------------------
#1 - Load the data
library(readr)

baseurl = "http://www.few.vu.nl/~molenaar/courses/statR/"
f.red <- file.path(baseurl, 'data', 'wine_quality', 'winequality-red.csv')
d <- read_delim(f.red, delim = ';')
# Alternative method:
#d <- read.csv(f.red, sep = ";", header = TRUE)

# --------------------------------------------------------------------
#2 - Center and normalize variables
library(dplyr)
library(tidyr)

# Create a unique ID for the table
d_norm <- d %>% 
  mutate(sampleID = 1:nrow(d))

head(d_norm)

# Gather the values per each feature
d_norm <- d_norm %>% 
  gather(key = variable,
         value = value,
         -sampleID)

head(d_norm)

# Calculate the normalized values (x = (x-mean) / std. dev)
d_norm <- d_norm %>% 
  group_by(variable) %>% 
  mutate(normvalue = (value - mean(value)) / sd(value))

head(d_norm)

# Remove 'value' and 'sampleID' and spread the table
d_norm <- d_norm %>% 
  select(-value) %>% 
  spread(variable, normvalue) %>% 
  select(-sampleID)

head(d_norm)


# --------------------------------------------------------------------
#3 - Split data into training and test (50/50)
set.seed(2)
trainset <- sample(rownames(d_norm), size = trunc(0.5 * dim(d_norm)[1]))
testset <- rownames(d_norm)[!(rownames(d_norm) %in% trainset)]
trainsamples <- d_norm[trainset, ]
testsamples <- d_norm[testset, ]

# Split predictor and target variables in train set
train.x <- trainsamples %>% 
  select(-quality) %>% 
  as.matrix()

train.y <- trainsamples$quality

# Split predictor and target variables in test set
test.x <- testsamples %>% 
  select(-quality) %>% 
  as.matrix()

test.y <- testsamples$quality


# --------------------------------------------------------------------
#4 - Make a pairplot to show correlations between features
library(GGally)

pairs(select(d_norm, -quality), pch = '.')
# or
ggpairs(select(d, -quality))



# --------------------------------------------------------------------
#5 - Fit the data
library(glmnet)

# Ridge regression
fit.ridger <- glmnet(train.x, train.y, alpha = 0)

# Lasso regression
fit.lassor <- glmnet(train.x, train.y, alpha = 1)

# Plot the results
par(mfrow=c(1,2))
plot(fit.ridger, xvar = "lambda")
plot(fit.lassor, xvar = "lambda")



# --------------------------------------------------------------------
#6

# Perform cross validation
set.seed(7)
cv.ridge <- cv.glmnet(x = train.x,
                      y = train.y,
                      alpha = 0)

cv.lasso <- cv.glmnet(x = train.x,
                      y = train.y,
                      alpha = 1)

# Plot the results
plot(cv.ridge)
plot(cv.lasso)


# Derive the lambda values
lambdas <- c(cv.ridge$lambda.1se,
             cv.lasso$lambda.1se)

# --------------------------------------------------------------------
#7 - Create final optimal models
library(broom)
library(tidyr)

final.ridge <- glmnet(x = train.x,
                      y = train.y, 
                      alpha = 0,
                      lambda = lambdas[1])

final.lasso <- glmnet(x = train.x,
                      y = train.y,
                      alpha = 1,
                      lambda = lambdas[2])

# Extract model parameters and present them in a table
coef.lasso <- tidy(final.lasso) %>% 
  select(term, estimate) %>% 
  mutate(coefficient = estimate) %>% 
  select(-estimate) %>% 
  filter(term!='(Intercept)')

coef.ridge <- tidy(final.ridge) %>%
  select(term, estimate) %>%
  mutate(coef.min=estimate) %>%
  select(-estimate) %>% 
  filter(term!='(Intercept)')

print(coef.lasso)
print(coef.ridge)


# --------------------------------------------------------------------
#8 - Calculate MSE of models on test set
set.seed(42)
ridge_pred <- as.data.frame(predict(final.ridge, test.x))
lasso_pred <- as.data.frame(predict(final.lasso, test.x))

# Add the actual values
ridge_pred$actual_quality <- test.y
lasso_pred$actual_quality <- test.y

# Add square errors
ridge_pred <- ridge_pred %>% 
  mutate(sq_error = (actual_quality - s0)^2)

lasso_pred <- lasso_pred %>% 
  mutate(sq_error = (actual_quality - s0)^2)


# Calculate Mean square error (MSE) of the models
n <- length(ridge_pred[, 1])

mse_ridge <- (1/n) * sum(ridge_pred$sq_error)
mse_lasso <- (1/n) * sum(lasso_pred$sq_error)

mse_ridge
mse_lasso

# -> The MSE are really similar


# --------------------------------------------------------------------
#8 - Alternative solution (maybe a bit fancier)

data.frame(quality=test.y, 
           ridge=predict(final.ridge, newx = test.x)[,1], 
           lasso=predict(final.lasso, newx = test.x)[,1]) %>%
  gather(key=model, value=prediction, -quality) %>%
  group_by(model) %>%
  summarize(mse = sum((prediction-quality)^2)/n())
