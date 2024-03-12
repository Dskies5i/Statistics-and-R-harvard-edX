install.packages("downloader")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)

RNGkind()

dat<-read.csv("femaleMiceWeights.csv")
#name of columns: "Diet" and "

#What is the entry in the 12th row and second column?
dat[12,2]
#You should have learned how to use the $ character to extract a column from a table and return it as a vector. 
#Use $ to extract the weight column and report the weight of the mouse in the 11th row.
dat$Bodyweight[11]

#The length() function returns the number of elements in a vector.
#How many mice are included in our dataset?
length(dat$Diet)
  
#To create a vector with the numbers 3 to 7, we can use seq(3,7) or, 
#because they are consecutive, 3:7. View the data and determine what 
#rows are associated with the high fat or hf diet. 
#Then use the mean() function to compute the average weight of these mice.

#What is the average weight of mice on the high fat diet?
mean(dat$Bodyweight[seq(13,24)])
mean(dat$Bodyweight[13:24])

#One of the functions we will be using often is sample(). 
#Read the help file for sample() using ?sample. 
#Now take a random sample of size 1 from the numbers 13 to 24 and report back 
#the weight of the mouse represented by that row. Make sure to type set.seed(1) 
#to ensure that everybody gets the same answer.

?sample
set.seed(1)
sample(dat$Bodyweight[13:24],1)

library(dplyr)

View(dat)

#select (select columns/variables) and filter (filter certain rows)

controls <- filter(dat, Diet=="chow")
View(controls)

controls<-select(controls, Bodyweight)

#make it a numeric list
unlist(controls)

#pipedown the process beind

controls <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(controls)
