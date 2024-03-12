###-----------------------------------------------------------------------------
##Normal distribution aka. bell curve aka. gaussian distribution
###-----------------------------------------------------------------------------

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

#Using the same process as before (in Null Distribution Exercises), set the seed at 1, 
#then using a for-loop take a random sample of 5 mice 1,000 times. Save these 
#averages. After that, set the seed at 1, then using a for-loop take a random sample
#of 50 mice 1,000 times. Save these averages:

set.seed(1)
mice5<-vector("numeric",n)
n<-1000
for(i in 1:n){
  mice5[i]<-mean(sample(x,5))
}



set.seed(1)
mice50<-vector("numeric",n)
n<-1000
for (i in 1:n){
  mice50[i]<-mean(sample(x,50))
}


#Normal Distribution Exercises #1
#Use a histogram to "look" at the distribution of averages we get with a sample 
#size of 5 and a sample size of 50. How would you say they differ?
#A: both look normal but sample size 50 has a smaller spread
hist(mice5)
hist(mice50)

#Normal Distribution Exercises #2
#For the last set of averages, the ones obtained from a sample size of 50, 
#what proportion are between 23 and 25?

1-mean(mice50<=23)-mean(mice50>=25)
#0.982
mean((mice50>=23)&(mice50<=25))
#0.982
mean(mice50>=23 & mice50<=25)
#0.982



?pnorm
mean(x)

pnorm(25, 23.9, 0.43)-pnorm(23, 23.9, 0.43)
#0.9765648
pnorm((25-23.9)/0.43)-pnorm((23-23.9)/0.43)
#0.9765648



###-----------------------------------------------------------------------------
##Population, Samples, and Estimates Exercises
###-----------------------------------------------------------------------------
library(downloader)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <-basename(url)
download(url, destfile=filename)
dat <-read.csv(filename)
dat

#remove missing values
dat<-na.omit(dat)
##Population, Samples, and Estimates Exercises #1
#Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
#What is this population's average?

chow<- filter(dat, Diet=="chow", Sex=="M") %>% select(Bodyweight) %>% unlist
chow
mean(chow)

##Population, Samples, and Estimates Exercises #2
#Now use the rafalib package and use the popsd() function to compute the male population standard deviation.
library(rafalib)
popsd(chow)
###A: 4.420501
#Population, Samples, and Estimates Exercises #3

#Set the seed at 1. Take a random sample  of size 25 from x.
#what is the average of the sample?

set.seed(1)
schow<-sample(chow,25)
mean(schow)
#A:30.5196

##Population, Samples, and Estimates Exercises #4

#Use dplyr to create a vector y with the body weight of all males on the 
#high fat hf) diet.
#What is this population's average?
dat<-na.omit(dat)
y<-filter(dat, Diet=="hf", Sex == "M")  %>% select("Bodyweight")%>% unlist
y
mean(y)
### A: 34.84793
#Population, Samples, and Estimates Exercises #5
#Now use the rafalib package and use the popsd() function to compute the population standard deviation.

popsd(y)
### A: 5.574609

#Population, Samples, and Estimates Exercises #6
#Set the seed at 1. Take a random sample  of size 25 from y.
#What is the sample average?
set.seed(1)
sy<-sample(y,25)
mean(sy)

#Population, Samples, and Estimates Exercises #7
#What is the difference in absolute value between samplemean(y) - samplemean(x) and popmean(y) - popmean(x) and ?

abs(mean(schow)-mean(sy))-abs(mean(chow)-mean(y))
##A: 1.399884

#Population, Samples, and Estimates Exercises #8
#Repeat the above for females, this time setting the seed to 2.
#What is the difference in absolute value between  and ?
#Make sure to set the seed to 2 before each sample() call. This function should be called twice.
dat
dat<-na.omit(dat)
fchow<-filter(dat, Diet=="chow", Sex=="F")%>%select("Bodyweight")%>%unlist
fchow
fhf<-filter(dat, Diet =="hf", Sex =="F" ) %>%select("Bodyweight")%>%unlist
fhf
set.seed(2)
sfchow<-sample(fchow,25)
set.seed(2)
sfhf <- sample(fhf,25)
abs(abs(mean(fchow)-mean(fhf))-abs(mean(sfchow)-mean(sfhf)))
### A: 0.3647172

n<-10000
vec<-vector("numeric",n)

for (i in 1:n){
  vec[i]<-mean(sample(fchow,100))
}
hist(vec)
sd(vec)
