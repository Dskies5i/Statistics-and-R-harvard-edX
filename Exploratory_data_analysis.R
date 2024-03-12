install.packages("UsingR")
library(UsingR)
x=father.son$fheight

hist(x)
bins <- seq(floor(min(x)),ceiling(max(x))) ##va de cm en cm
hist(x,breaks=bins,xlab="Height",main="Adult men heights")

?seq
#For any number x the empirical CDF reports the proportion of numbers in our 
#list smaller or equal to x
#empirical cumulative density function (CDF) -> ecdf()
myCDF <- ecdf(x) 
myCDF
## evaluate the function at these values:
  xs<-seq(floor(min(x)),ceiling(max(x)),by=0.1) 
### and then plot them:
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")

##--------------------------------------------------------------------------
#we can use the sd() for an estimate of the standard deviation (σ) function or 
#caculate  the population sd (σ)

popsd <- function(x)  sqrt(mean((x-mean(x))^2))

popsd(x) # 2.743595
sd(x) #2.744868
#both results approximate so, it indicates normal distribution

#we cal also check if the porportion of people with more or less height still agree
mean(x>70) #0.2059369
1-pnorm(70, mean(x), sd(x)) #0.1997182

mean(x<60) #0.003710575
pnorm(60, mean(x), sd(x)) #0.002550908
#we suspect normal distribution is good approximation

##--------------------------------------------------------------------------
#but we can make a quantile plot, first by "hand"

ps <-seq (0.01, 0.99, 0.01)
qs <- quantile(x,ps)
normalqs<- qnorm(ps,mean(x), sd(x))
plot(normalqs, qs, xlab="Normal percentiles", ylab="Observed percengiles (heights)")
abline(0,1) ##identity line"

#the graph and normal distribution predicts lines very well

#now with the function is easier

qqnorm(x)
qqline(x)
#it makes it authomatically
##--------------------------------------------------------------------------
#Download this RData file to your working directory. Then load the data into R with the following command:
load("~/Documents/R projects/edX course/skew.RData")

dim(dat)
dat
#Using QQ-plots, compare the distribution of each column of the matrix to 
#a normal. That is, use qqnorm() on each column. To accomplish this quickly, 
#you can use the following line of code to set up a grid for 3x3=9 plots. 
#(mfrow means we want a multifigure grid filled in row-by-row.Another choice is mfcol.)

par(mfrow = c(3,3),mar = c(1, 1, 1, 1))

#Then you can use a for loop, to loop through the columns, and display one 
#qqnorm() plot at a time. You should replace the text between ** with your own code.
for (i in 1:9) {
  print(i)
  qqnorm(dat[,i])
}

#4 and 9 are skewed
##--------------------------------------------------------------------------

#The InsectSprays data set measures the counts of insects in agricultural experimental units treated with different insecticides. This dataset is included in R, and you can examine it by typing:
head(InsectSprays)

#Try out two equivalent ways of drawing boxplots in R, using the InsectSprays dataset. Below is pseudocode, which you should modify to work with the InsectSprays dataset.

#1) using split: boxplot(split(values, factor))
boxplot(split(InsectSprays$count, InsectSprays$spray))
#2) using a formula: boxplot(values ~ factor)
boxplot(InsectSprays$count ~ InsectSprays$spray)

#question 1: Which spray seems the most effective (has the lowest median count)? A: C
#question 2: Which are the groups with visible outliers? A: C, and D

#Let's consider a random sample of finishers from the New York City Marathon in 2002. This dataset can be found in the UsingR package. Load the library and then load the nym.2002 dataset.

library(dplyr)
data(nym.2002, package="UsingR")
#question 3: Use boxplots and histograms to compare the finishing times of males and females. Which of the following best describes the difference?
#A: Male and females have similar right skewed distributions with the former, 20 minutes shifted to the left.
nym.2002
boxplot(nym.2002$time ~ nym.2002$gender)
par(mfrow = c(1,2))
fem <- filter(nym.2002, gender=="Female") %>% select(,time) %>% unlist
fem
bins2 <- seq(floor(min(fem)),ceiling(max(fem)))
hist(fem, breaks=bins2, xlab="Fem", main="Racing time females")
qqnorm(fem)
qqline(fem) #not normal

male <- filter(nym.2002, gender=="Male") %>% select(,time) %>% unlist
male
bins3 <- seq(floor(min(male)),ceiling(max(male)))
hist(male, breaks=bins3, xlab="Male", main="Racing time males")
qqnorm(male)
qqline(male) #not normal

mean(male)
mean(fem)
