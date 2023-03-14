## Chad Williams Get R Done tutorial
# https://www.youtube.com/watch?v=pJOhYPBYn5U&list=PLmNgrNF3pZqg2GPmcIAWDa1A2-cAfbX37&index=10

#### Episode 10 - Packages ####

# a package has a bunch of functions in it

#install the package... I don't think I need to do this each time
install.packages('ggplot2')
#load the package
library(ggplot2) #check mark in Packages means the package is loaded

#### Episode 11 - Commenting and Organizing the script ####

# "#" is a way to include comments in the script
# can also include sections... four #'s make an arrow that you can collapse the section

#### Episode 12 - Short and Long Data Formats ####

# load data into the workspace
short_data=read.csv('matrix_example.csv',row.names = 1)
# re-name col names
colnames(short_data) = c('Lecturer','Flipped','Applied') #this is short format

#change into long format
install.packages('reshape2')
library(reshape2)

long_data = melt(short_data) #this is long format... this is important for adding more variables (e.g., time of day)

#add a new column to short_data
time=c(rep('Early',5),rep('Late',5))
short_data=cbind(short_data,time)

#re-melt the data to long format... two ways to do this
# 1st way
long_data = melt(short_data) #added more variables (e.g., time of day)

# 2nd way
long_data = melt(short_data,id='time')

#### Episode 13 - Conditional Arguments ####

# remove everything in the global environment
rm(list = ls())

# create a variable
numbers=26:35

# look for number in a location (2nd place)
numbers[2]

# look for a specific number
numbers ==28 #this returns logicals

# look for two specific numbers
(numbers ==28) | (numbers==30)

# look for numbers greater than or equal to 30
numbers>=30

# look for numbers between numbers
(numbers>29)&(numbers<31)

# look for numbers between numbers or a specific number (| symbol = or)
(numbers>29)&(numbers<31) | (numbers==35)

#remove the variable
rm(numbers)

#### Episode 14 - How to Select Data ####

# import data to use
short_data=read.csv('matrix_example.csv',row.names = 1)
colnames(short_data) = c('Lecturer','Flipped','Applied') #this is short format; re-name col names
#add a new column to short_data
time=c(rep('Early',5),rep('Late',5))
short_data=cbind(short_data,time)
#re-melt the data to long format... two ways to do this
#change into long format
library(reshape2)
long_data = melt(short_data) #this is long format... this is important for adding more variables (e.g., time of day)

# create a new variable for early data only
early_index=long_data[,1]=='Early' #indexes the Early data
early_data=long_data[early_index,] #this removes all rows that do not have "Early" in them

# create a new variable with Lecturer and Flipped data
LecturerFlipped_index=(long_data[,2] == 'Lecturer') | (long_data[,2] == 'Flipped')
LecturerFlipped_data=long_data[LecturerFlipped_index,]

# create a new variable with lecturer and early data
LecturerEarly_index=(long_data[,2]=='Lecturer') & (long_data[,1]=='Early')
LecturerEarly_data=long_data[LecturerEarly_index,]


#### Episode 15 - Rough Plots to Check Data ####

#clear global environment
# remove everything in the global environment
rm(list = ls())

# create a matrix
data=cbind(rnorm(100,10,4),rnorm(100,20,6)) #cbind binds two arrays together, rnorm creates a random array with a number of rows, mean, and SD
View(data)

# add col names
colnames(data)=c('Lecturer','Flipped')

#create a histogram of the second column
hist(data[,2])
#create a histogram of the first column
hist(data[,1])

#create a histogram of the column named flipped
hist(data[,'Flipped'])

# subplot in a 1 by 2 figure
par(mfrow=c(1,2))
hist(data[,1])
hist(data[,2])

# create a scatterplot
plot(data) #only works because there's only 2 columns

# create new variable with 3 variables
data3=cbind(rnorm(100,10,4),rnorm(100,20,6),rnorm(100,15,5))
colnames(data3)=c('Lecturer','Flipped','Both')
# plot columns 2 and 3
plot(data3[,2:3])

# create a barplot
barplot(colMeans(data3)) #barplot only plots the means

# create a boxplot
boxplot(data3)

# create a line plot
plot(colMeans(data3)) #plots the means of the three columns
lines(colMeans(data3)) #creates lines between the three markers 


#### Episode 16 - Descriptive Statistics ####
#clear global environment
rm(list = ls())
# load in data to use
short_data=read.csv('matrix_example.csv',row.names = 1)
colnames(short_data) = c('Lecturer','Flipped','Applied') #this is short format

#install the psych package for statistics functions
install.packages('psych')
library(psych)

#calculate and show the descriptive statistics of the short_data dataset
describe(short_data)

#put the descriptive stats into a variable
descriptive_statistics=describe(short_data)

# find the mean of the Lecturer (row 1)
descriptive_statistics[1,]

# this method only works for short data... need to do it differently for long data
library(reshape2)
long_data=melt(short_data)
describe(long_data) #this assumes the columns are just columns and not separated by descriptors

# can get stats from long data another way
lecturer=long_data[long_data[,1]=="Lecturer",]
mean(lecturer[,2])
sd(lecturer[,2])
median(lecturer[,2])
min(lecturer[,2])
max(lecturer[,2])
length(lecturer[,2]) # use length instead of "n"
standard_error=sd(lecturer[,2])/sqrt(length(lecturer[,2])) # calculate the SE
quantile(lecturer[,2])





#### Episode 17 - Assumption of Normality ####
# clear global environment
rm(list = ls())
#create matrix to use
data=cbind(rnorm(100,10,4),rnorm(100,20,6),rnorm(100,5,8))
colnames(data) = c('Lecturer','Flipped','Applied') #this is short format

#look at data with plots first
par(mfrow=c(2,3)) #create a subplot
hist(data[,1]) #create a histogram with data from the first column
hist(data[,2])
hist(data[,3])

#visually check the q-q plots
qqnorm(data[,1]) #create a normal q-q plot
qqline(data[,1]) #insert line on the plot
qqnorm(data[,2])
qqline(data[,2])
qqnorm(data[,3])
qqline(data[,3])

#check normality objectively with a Shapiro-Wilks test (check p-values)
shapiro.test(data[,1])
shapiro.test(data[,2])
shapiro.test(data[,3])


#### Episode 18 - Assumption of Homogeneity of Variance ####

#create subplot w histograms
par(mfrow=c(1,2))
min1=min(data[,1]) #check to see min and max to pick appropriate x axis limits
max1=max(data[,1])
min3=min(data[,3])
max3=max(data[,3])
hist(data[,1],xlim=c(-25,25)) # x axis limits are equal
hist(data[,3],xlim=c(-25,25)) # x axis limits are equal

#use levene's test to check homogeniety
install.packages("car")
library(car)
library(reshape2)
long_data=melt(data[,c(1,3)]) #put data into long format

leveneTest(long_data[,3]~long_data[,2]) # y is our 3rd column which is our value, x is our 2nd column which is our variable name




#### Episode 19 - One-sample t-test ####
#clear global environment
rm(list = ls())
#create data matrix to use
data=rnorm(50,5,2)
hist(data)

#t-test: is the data different than 0? It shouldn't be because the mean of data is 5
t.test(data, mu=0)

#store the t-test results in a variable
ttest_result=t.test(data, mu=0)
