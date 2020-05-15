#---------------------------------#
# 24.1 Exercises
# 1. Read data
baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "straindiversity.dif"
url <- file.path(baseurl, 'data', 'strain_diversity', filename)
d <- read.DIF(file = url,
                             header = TRUE,
                             na.strings = 'nd')

head(d)
summary(d)

#---------------------------------#
# Description of the data

# Strain & Other.Strain.Code = Strain code
# Origin = 2 levels (dairy / non-dairy)
# TaxonomicPosition = 3 levels (cremoris, lactis, lactis var diacetylactis)
# Variant = 3 levels (cremoris, diacetylactis, lactis)
# Cremoris.Like.Genotype = 0/1 -> FALSE / TRUE

#GM... columns = activity measurements (numbers)
  # -> 2 mediums: GM17 or CDM

#---------------------------------#
# Data conversion

# Exercise 1
d$Cremoris.Like.Genotype <- as.logical(d$Cremoris.Like.Genotype)

#Exercise 2
GM17 <- 8:12
CDM <- 13:17

#Exercise 3
apply(d[, c(GM17, CDM)], 2, class)

# Exercise 4
d[, c(GM17, CDM)] <- apply(d[, c(GM17, CDM)], 2, as.numeric)
apply(d[, c(GM17, CDM)], 2, class)


#---------------------------------#
# Data analysis part 1

# Exercise 1
# Make a pairplot of all the enzyme activity columns (8:17)
plot(d[, c(GM17, CDM)], pch = 20)
plot(log(d[, c(GM17, CDM)]), pch = 20)

# Exercise 2
# Calculate a correlation matrix of the enzyme activity data
# using both Pearson and Spearman rank correlation definitions
cor_p <- cor(d[, c(GM17, CDM)], method = c("pearson"), use = 'complete.obs')
cor_s <- cor(d[, c(GM17, CDM)], method = c("spearman"), use = 'complete.obs')

# Exercise 3
# Hierarchical tree
# Distance: 1 - correlation coefficient
# clustering: hclust()

# Calculate distance (1 - correlation matrix)
distance <- as.dist(1 - cor_s)

# Perform hiearachical clustering
hc <- hclust(distance,
             method = "ward.D")

# Plot results
par(mfrow=c(1,1))
plot(hc, hang = -1)

# Exercise 4
# create vector of colors
mycolors <- c('red', 'green', 'blue')

plot(log(d[, GM17, CDM]),
     cex = 0.5,
     col = mycolors)

plot(log(d[,c(GM17,CDM)]), pch=20, col=c('red','green')[d$Origin])
plot(log(d[,c(GM17,CDM)]), pch=20, col=c('red','green')[d$Cremoris.Like.Genotype+1])


#---------------------------------#
# Data analysis part 2: Decision trees

# Exercise 1
install.packages("party")
library(party)

irisct <- ctree(Species ~ . , data = iris)
plot(irisct)
table(predict(irisct), iris$Species)

# Exercise 2
origtree <- ctree(Origin ~ ., data = d[, c(4, GM17, CDM)])

# Exercise 3
table(predict(origtree), d$Origin)
pred_table <- cbind(as.character(d$Origin),as.character(predict(origtree)))

# Exercise 4
View(table(interaction(d$Origin, d$Variant)))


# Exercise 5
correct <- sum(pred_table[,1] == pred_table[, 2]) / (length(pred_table) / 2)
correct

library("caret")
# Define training control
train.control <- trainControl(method = "LOOCV")

# Train the model
model <- train(Origin ~., 
               data = d[, c(4, GM17, CDM)],
               method = 'rpart',
               trControl = train.control,
               na.action = na.pass)

print(model)



















