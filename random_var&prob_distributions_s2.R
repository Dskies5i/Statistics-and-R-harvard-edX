library(dplyr)
mice <- read.csv("femaleMiceWeights.csv")
mice

control <- filter(mice, Diet=="chow") %>% select(Bodyweight) %>% unlist
control

treatment <- filter(mice, Diet =="hf" ) %>% select (Bodyweight) %>% unlist
treatment

#compare the control to the treatment 

mean(control) #23.81333
mean(treatment) #26.83417
sd(control) #3.022541
sd(treatment) # 4.097606
obsdiff <- mean(treatment) - mean(control)
print(obsdiff) #3.020833

#how can we know that the difference between the means from the control and
#the treatment are precise and not meere chance? In this case we have access 
#to the population, so we can take other random variables (samples) and check
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population <- read.csv(filename)

population <- unlist(population) # turn it into a numeric
population

#lets sample 12 mice 3 times and see how the average changes
control <- sample(population,12)
mean(control)
#23.46167
control <- sample(population,12)
mean(control)
#23.68167
control <- sample(population,12)
mean(control)
#23.85083


#how do we know that the difference in means (obsdiff) is due to the diet?
#if we repeat the experiment will we see this difference? this is called 
#null hypothesis. "Null" meaning that there is a null effect, no difference

#since we have access to the population, we can observe as many values as
#we want of the difference of the averages when the diet has no effect.
#We can do this by ramdomly sampling 24 control mice, giving them
#the same diet, and then recording the difference in mean berween the 2
#randomly split groups of 12 and 12. Here it is the code in R:

##12 control mice
control <- sample(population,12)
##another 12 control mice that we act as if they were not
treatment <- sample(population,12)
print(mean(treatment) - mean(control))
#0.1833333

n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}
max(null)
#4.728333
hist(null)

#the values in the vector null are the null distribution. So, what percent
#of the 10,00 are bigger than obsdiff?

mean(null>= obsdiff)
#0.0138 this is the pvalue


#only a small percent of the 10,00 simulations. What do we skeptically 
#conclude? thwn there is no diet effect we see a diference as big
#as the one observed oonly 1.5% of the time. This is what is know as
#p-value

###-----------------------------------------------------------------------------
##Random Variables Exercises
###-----------------------------------------------------------------------------

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
x
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
#1.-What is the average of these weights?

mean(x)
#mean=23.89338

#2.-Take a random sample of size 5. What is the absolute value (use abs()) of 
#the difference between the average of the sample and the average of all the values?
set.seed(1)
sample <-sample(x, 5)
sample
abs(mean(sample)-mean(x))
#0.3293778

#3.-After setting the seed at 5, set.seed(5), take a random sample of size 5. 
#What is the absolute value of the difference between the average of the sample and the average of all the values?
set.seed(5)
sample <-sample(x, 5)
sample
abs(mean(sample)-mean(x))
#0.3813778

###-----------------------------------------------------------------------------
##Intro to null distribution
###-----------------------------------------------------------------------------
x<- read.csv("femaleControlsPopulation.csv")
### Null Distributions Exercises #1
x<- unlist(x)
#What proportion of these 1,000 averages are more than 1 gram away from the 
#average of x ? Proportions are written as numbers between zero and one.
n<-1000
null <- vector("numeric",n)
set.seed(1)
for (i in 1:n){
  null[i]<-mean(sample(x,5))
}
hist(null)
mean(x)
mean(abs(null-mean(x))>1) #uses absolutes to include numbers on the positive
#and negative sides in the histogram
#  0.503

### Null Distributions Exercises #2

#We are now going to increase the number of times we redo the sample from 1,000 to 10,000. Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. Save these averages.

#What proportion of these 10,000 averages are more than 1 gram away from the average of x ?

n<- 10000
null <-vector("numerical",n)
set.seed(1)
for (i in 1:n){
  null[i]<-mean(sample(x,5))
}
hist(null)
mean(null)
mean(abs(null-mean(x))>1) 
#null goes like that because it is already a mean, and mean (x) is a constant
#absolute values to take into accoun both tails of the histogram
# 0.5084


###-----------------------------------------------------------------------------
##Probability Distribuitions Exercises
###-----------------------------------------------------------------------------
install.packages("gapminder")
library(gapminder)
library(dplyr)
data(gapminder)
head(gapminder)
n_distinct(gapminder$country) #142 different countries
x<-filter(gapminder, year=="1952") %>% select(lifeExp) %>% unlist
hist(x)


#In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.
#We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal to a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.

#The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.

#Let's continue, using the simpler mean() function.

#What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(x<=40)
#0.2887324

?sapply
#Suppose we want to plot the proportions of countries with life expectancy q 
#for a range of different years. R has a built in function for this, plot(ecdf(x)), 
#but suppose we didn't know this. The function is quite easy to build, by 
#turning the code from question 1.1 into a custom function, and then using 
#sapply(). Our custom function will take an input variable q, and return the 
#proportion of countries in x less than or equal to q. The curly brackets, 
#{ and }, allow us to write an R function which spans multiple lines:

#The sapply() function helps us in applying functions on a list, vector, or 
#data frame and returns an array or matrix object of the same length.

prop<- function(q){
  mean(x<=q)
}
prop(40)
#0.2887324

#Now let's build a range of qs that we can apply the function to with sapply():

qs=seq(from=min(x), to=max(x), length=20)

#Print qs to the R console to see what the seq() function gave us. Now we 
#can use sapply() to apply the prop function to each element of qs:
qs
props = sapply(qs, prop)
#Take a look at props, either by printing to the console, or by plotting 
#it over qs:
plot(qs, props)

#you can also define the props function inside sapply():

props = sapply (qs, function(q) mean(x<=q))
plot(qs, props)

#all of this is what our ecdf(x) does
plot(ecdf(x))

