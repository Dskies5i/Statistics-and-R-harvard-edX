library(downloader)
library(dplyr)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

#dplyr Exercises #1
#Read in the msleep_ggplot2.csv file with the function read.csv() and use the 
#function class() to determine what type of object is returned.
sleep <- read.csv("msleep_ggplot2.csv") 
class(sleep) #data.frame
View(sleep)

#dplyr Exercises #2
#Now use the filter() function to select only the primates.
#How many animals in the table are primates?
#number of the animals that are primates

prim <-filter(sleep, order =="Primates")
nrow(prim)
#dplyr Exercises #3
#What is the class of the object you obtain after subsetting the table to only 
#include primates?
class(prim)

#dplyr Exercises #4
#Now use the select() function to extract the sleep (total) for the primates.
#What class is this object?
primsleep <-select(prim, sleep_total)
primsleep
class(primsleep)


#dplyr Exercises #5
#average of primate sleep. Transform data frame to vector with unlist
#Now we want to calculate the average amount of sleep for primates (the average of 
#the numbers computed above). One challenge is that the mean() function requires 
#a vector so, if we simply apply it to the output above, we get an error. 
#Look at the help file for unlist() and use it to compute the desired average.

#What is the average amount of sleep for primates?
  
meanprimsleep <- unlist(primsleep) %>% mean
meanprimsleep

#Exercise 6
#For the last exercise, we could also use the dplyr summarize() function. 
#We have not introduced this function, but you can read the help file and 
#repeat exercise 5, this time using just filter() and summarize() to get the answer.
#What is the average amount of sleep for primates calculated by summarize()

?summarize
meanprimsleep2 <- filter(sleep, order =="Primates") %>% summarize(mean(sleep_total) )
meanprimsleep2
