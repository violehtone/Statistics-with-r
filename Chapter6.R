#6.1-6.4
setwd("/home/villelehtonen/Desktop/statw-r/IO")

#try out read table function
read.table("scores.txt")
read.table("scores.txt", sep="\t")
read.table("scores.txt", sep="\t", 
           header=TRUE, 
           quote='',
           comment.char = '#',
           na.strings = NA)


d <- read.table("scores.txt", sep="\t", header=TRUE)
dim(d) # dimensions
class(d) #class of table
ls.str(d) #class of columns

#add new column
d$test.4 = c(2.5, 8, 7, 8)
head(d)

#write.table(d, file="scores_nw.txt")
#write.table(d, file="scores_nw.txt", quote=FALSE)
#write.table(d, file="scores_nw.txt", quote=FALSE, sep="\t")
write.table(d, file="scores_nw.txt", quote=FALSE, sep="\t", row.names=FALSE)

library(tibble)
irist <- as_tibble(iris)

library(readr)
read_tsv("scores.txt", comment="#")
write_tsv(d, path="scores_new.txt")

#6.5
baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
f <- file.path(baseurl, 'data', 'io', 'weights.txt')
d <- read_tsv(f)

d <- read.table(f, sep="\t", header=TRUE)
head(d)

#6.6
setwd("/home/villelehtonen/Desktop/statw-r/")
f <- file.path(baseurl, 'data', 'io', 'arrays.zip')

if(grepl('http:', baseurl)) {
  download.file(f, destfile = 'arrays.zip')
}else{
  file.copy(f, getwd())
}

#read files inside the zip
con <- unz('arrays.zip', filename = 'array_1.txt')
d <- read_tsv(con)
d

#alternative way of reading
con <- unz('arrays.zip', filename = 'array_1.txt')
d <- read.table(con, header=TRUE, sep="")
head(d)

#Remove the arrays.zip file
unlink('arrays.zip')










