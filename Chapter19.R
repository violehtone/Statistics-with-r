# Exercise 1
############################
baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "hubble.txt"

url <- file.path(baseurl, 'data', 'hubble', filename)
d <- read.table(url, sep = '\t', header = T)
head(d)

plot(y~x, 
     data = d,
     xlab = "distance (Mpc)",
     ylab = "Velocity (km/s)")


# Exercise 2
############################
# y = Bx
model <- lm(y ~ 0 + x,
            data = d)

summary(model)
b <- model$coefficients / (3.09*10^19)

# age = 1/b
age <- 1/ (b * 3600*24*365)
age


# Exercise 3
############################
# Detect outliers
residuals <- model$residuals
hist(residuals)
sort(residuals)
#-> outliers are 15 and 3

# Remove outliers
d_new <- d[-c(3,15), ]
model_new <- lm(y ~ 0 + x,
                data = d_new)
b_new <- model_new$coefficients / (3.09*10^19)
age_new <- 1 / ((b_new * 3600*24*365)) 
age_new

#difference
age - age_new



# Exercise 4
############################
# b +- 2 * o
res.hubfit <- summary(model)
hubble.confint <- (res.hubfit$coefficients['x','Estimate'] + c(-1,1)*res.hubfit$coefficients['x','Std. Error'])/(3.09*10^19)
hubble.confint2 <- confint(model, level=0.95)/(3.09*10^19)

# print results
1/(hubble.confint*3600*24*365)
1/(hubble.confint2*3600*24*365)



