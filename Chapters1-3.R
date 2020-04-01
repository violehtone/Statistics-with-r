### CHAPTER 2
#Basic functions 
exp(5) #e^5
log(5,2) #log to base n of x
floor(5.2) #-> 5
ceiling(5.2) #->6
round(1.234567,3) #->1.234
runif(10) #-> 10 x random numbers u(0,1)
pi*2^2 # area of circular surface

#Exercises
((4/6)*8-1)^(2/3)
log(20)
log(4096,2)
2*pi*3
exp(2+cos(0.5*pi))
sqrt(2.3^2 + 5.4^2 - 2*2.3*5.4 * cos(pi/8))

#listing objects
a <- 5
b <- 3
c <- c(1,2,3)
ls() # <- list all objects (names) stored in memory
ls.str()  #<- list all objects and values stored in memory


### CHAPTER 3
#vectors
c(12,9,3,22)
c(TRUE,FALSE,TRUE)

#numeric vectors
1:10
c(1:10)
seq(1,10)

seq(0,10, by=0.1) # 0.1, 0.2, ..., 10.0
rep(1:3,3) # 1,2,3,1,2,3,1,2,3

rep(1:3, each=2) #<- 1,1,2,2,3,3

c(c(1,2),c(3,4)) #concate two vectors
runif(10) #10 random numbers
runif(10, min=5, max=10) #random numbers in range 5-10

#character vectors
letters
LETTERS

#Logical vectors
1>2
!(1>2)

#Exercise
randv <- runif(20)
logicalv <- randv > 0.5

#Special values
1/0 #-> Inf
0/0 #-> NaN (Not a Number)
NA #-> NA (Not available)

c(2,NA,1,0)>1
c(1,2,NA,3)/2

#Selecting parts of vector
weights <- c(60,56,87,67,74)
weights[2]

#Selecting multiple indexes with a vector
maleindexes <- c(1,3,5)
weights[maleindexes]

#Calculate mean of specific indexes (i.e. men/women)
mean(weights[maleindexes])
mean(weights[-maleindexes])
mean(weights)

#select indexes by logical vector
maleindexes_logical <- c(TRUE,FALSE,TRUE,FALSE,TRUE)
weights[maleindexes_logical]

sex <- c('M','F','M','F','M')
sex == 'M'
mean(weights[sex == 'M'])


# Factors and matrix/array vectors
mydata <- c('Male','Female','Female','Male')
sex <- factor(mydata) #-> [1] Male Female Female Male
                      # levels: Female, Male

as.numeric(sex)

a <- 1:8
mymatrix <- matrix(a, ncol=2)
mymatrix
dim(mymatrix)

mymatrix2 <- matrix(a, ncol=2, byrow=TRUE)
mymatrix2

#array
myarray <- array(a,dim=c(2,2,2))
myarray

#Data frames
d <- data.frame(sex=c('F','M','F','M'),
                weight=c(52,86,65,70),
                age=c(28,36,21,53))

#lists
great_comet <- list(
  meta = list(observer = 'Tycho Brahe', 
              year = 1577,
              day0 = 'Nov 11',
              location = c(country='Denmark', place='Hven', observatory='Uraniborg'),
              object = 'comet'),
  data = data.frame(
    day = c(0,2,3),
    hour = c(17,5,17),
    angle_to_capricorn = c(NA,'21.40','18.30'),
    angle_to_aquila = c(NA,'26.50','23.45')
  )
)

## Quierying class of objects
class(great_comet) #<- class of object
class(d)
ls.str(great_comet) #<- objects and its values
ls.str(d)

#Selecting parts of data frames
numfiltered <- d[c(1,3)]
filtered <- d[c("sex","age")]
class(filtered) #-> data.frame
class(numfiltered) #-> data.frame
class(great_comet['data']) #-> list
class(great_comet['data']['hour']) #-> list


#Exercises
a <- c(1,2,3)
a/2
b <- c(4,5,6)
a*b
c <- c(4,5)
a*c
d <- c(1,2,3,4)
d*c
b <- c(2,0,3)
a/b

#Exercises
d <- data.frame(inhabitants=c(1000,200000,500),
                area=c(200,5000,NA))

d$density <- d$inhabitants / d$area
d

#Exercise
a <- array(1:8, dim=c(2,4))
b <- c(4,5)
a*b

#Matrix multiplication
a <- matrix(c(1:8), ncol = 2)
b <- t(matrix(c(10:17), ncol=2))

a%*%b

#Exercise
# Replace all Na values with zeros
a <- 1:10
a[3] = NA
a[5] = NA
logi <- c(is.na(a))
a[logi] <- 0

#2 ways to create the same matrix
a <- array(9:1, dim=c(3,3))
a <- matrix(9:1, ncol=3)
a[2:4]
a[c(1:3)]
a[1]
a[1,2]

#Exercise
a[3,c(1,3)] #3,1 & 3,3
a[3,] #3rd row
a[,3] #3rd column

a[2,2] <- NA
a[,c(2,3)]
d
d[3,]
class(d[3,])


## Removing elements from objects
v <- list(name='John', children=c('John jr.', 'Nancy'))
v[['children']] <- NULL

d <- data.frame(age=c(20,30,40),
                weight=c(55,81,76))
d$weight <- NULL
d

#vectors
a <- 1:4
a[a>2]

#arrays
a <- matrix(1:9, ncol=3)
#remove second row
a <- a[-2,]

## Data class conversion
d <- c('male','female','male','male')
class(d)
d<-as.factor(d)
class(d)

a <- 1:4
class(a)
a <- as.character(a)
class(a)

o <- c(F,T,F)
as.numeric(o)

o <- c(T,F,F,T,T)
sum(o[T]) #number of trues
sum(!o[T]) #number of falses



#### 3.10 - Exercises
#1
a <- 1:100
sub <- a[20:25]
class(sub)

#2
logi <- a<20
a[logi]

#3
a[round(a/2) == a/2]
a[!round(a/2) == a/2]

#4
n <- 200
d <- data.frame(age=round(12+70*runif(n)),
                gender=factor(ifelse(runif(n)>0.5,'male','female')),
                smoker=ifelse(runif(n)>0.8, TRUE,FALSE)
                )
#min = 12, max = 82

#5
dnew <- d[c("gender", "smoker")]
class(dnew)

#6
d[d$gender == 'female',]

#7
d[d$gender == 'female' & d$age>50 & d$smoker==T, ]

#9
class(d$smoker)
class(d["smoker"])
class(d[3])

#10
head(d)
length(d[d$smoker == T,"smoker"])
length(d$smoker[d$smoker == T])

#11
sum(d$smoker == T)

#12
min(d$age)
max(d$age)
mean(d$age)
median(d$age)

#13
summary(d)
plot(d)

#14
d[c(11:100),]

#15,16
servedsteaks <- factor(c('rare','medium','done')[trunc(runif(30)*3)+1])
steaks1 <- servedsteaks[!servedsteaks == "done"]

#17
levels(steaks1)
droplevels(steaks1, "done")


#18,19
flintstones <- list(
  person1=list(name='Fred',
               kids=c('Pebbles'),
               married=TRUE,
               pets=c('Dino','Baby Puss','Doozy')),
  person2=list(name='Barney',
               kids='Bamm-Bamm',
               married=TRUE,
               pets='Hoppy'))

class(flintstones$person2)
flintstones["person2"]
flintstones[["person2"]]

#21
d <- c(101:200)
s <- sample(d, 50)
sample(d)
length(sample(d))

t <- d[!(d %in% s)]
all(d %in% union(t,s))


### MORE EXERCISES
#1
v <- seq(from=16, to=56, by=2)
sum(v)
v[1:4]
prod(v[1:4])

a <- c(1,2,3,4)
prod(a)

v[c(4,9,11)]

#2
x <- c(2,9,0,2,7,4,0)
y <- c(3,5,0,2,5,4,6)
y/x
x>y
x==0
y[y > x]
y[y-y == x]
y[x==0]

y[x != 0]
x[x>=7] <- 0
x

#5
a <- matrix(c(3,9,7,4), ncol=2, byrow=T)
a
a <- matrix(c(1,2,3,4,5,6,7,8,9), ncol=3, byrow=T)
b <- matrix(rep(1,9), ncol=3)
A <- b/a
t(A)

B <- A[1:2,1:2]

D <- diag(1:3)
D <- cbind(D, c(4,4,4))
D <- rbind(D, c(5,5,5,5))
D

#6
attributes(A)
dim(A)
as.vector(A)
A[1:3]
