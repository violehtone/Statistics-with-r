# Chapter 25 - logistic regression using many predictors

# Data set: phenotype and sequence count data

# -----------------------------------------------------------------
# Exercise 1
# Read data
baseurl = "http://www.few.vu.nl/~molenaar/courses/statR/"
f.phenotype <- file.path(baseurl, 'data', 'Zeller2014', 'zeller2014_phenodata.csv')
f.seq <- file.path(baseurl, 'data', 'Zeller2014', 'zeller2014_sequencedata.csv')

df_pheno <- read_delim(f.phenotype, delim = ',')
df_seq <- read_delim(f.seq, delim = ",")

#head(df_pheno)
#head(df_seq)

# Join disease status from pheno to seq
library(readr)
library(dplyr)
library(tidyr)

df <- df_seq %>% 
  inner_join(df_pheno, by = "sampleID") %>% 
  select(sampleID, disease, feature, percentage)

# Spread table to contain disease column (response) and 
# column for every bacterial species (predictors)
df <- df %>% 
  spread(key = feature, value = percentage) %>% 
  select(-sampleID)


# -----------------------------------------------------------------
# Exercise 2
# Mutate disease to binary vector
df.binary <- df %>% 
  mutate(disease = recode(disease,
                          "n" = "0",
                          "cancer" = "1"))

df.binary$disease <- as.numeric(df.binary$disease)

# Fit a generalized linear model (glm)
model <- glm(disease ~ ., data = df.binary, family = 'binomial')

# -----------------------------------------------------------------
# Exercise 3
library("glmnet")
# Fit a general linear model
glmfit <- glmnet(x = select(df, -disease) %>% as.matrix(),
                 y = df$disease,
                 family = "binomial")

# Look at prediction performance with Lambda = 0.04
predTrsf <- predict(glmfit,
                    newx = select(df, -disease) %>% as.matrix(),
                    type = "class",
                    s = 0.04)

table(predTrsf, df$disease)

# diagnostic plot
plot(glmfit,
     lwd = sqrt(3),
     ylab = "")


# Do cross validation to estimate lambda
par(mfrow=c(1,1))
cvglmfit <- cv.glmnet(x = select(df, -disease) %>% as.matrix(),
                      y = df$disease,
                      family = "binomial")

plot(cvglmfit)

# Optimal lambda (minimum)
cvglmfit$lambda.min

# Largest value of Lambda such that performance measure is within 1 std. error of the min.
cvglmfit$lambda.1se

# Confusion table for lambda.1se
s0 <- cvglmfit$lambda.1se
predict(glmfit, 
        newx = select(df, -disease) %>% as.matrix(),
        type = "class",
        s = s0) %>% 
  table(df$disease)


# Find out which features drive the classification!
coefs <- coef(glmfit)[, which.min(abs(glmfit$lambda - s0))]
topthree <- order(abs(coefs), decreasing = TRUE)[1:3]
as.vector(coefs[topthree])

# Transform the data with asinh
cv.glmnet(x = asinh(select(df, -disease) %>% as.matrix()),
          y = df$disease,
          family = "binomial") %>% 
  plot
