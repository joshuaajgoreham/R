## Chad Williams Get R Done tutorial
# https://www.youtube.com/watch?v=pJOhYPBYn5U&list=PLmNgrNF3pZqg2GPmcIAWDa1A2-cAfbX37&index=20

#### Episode 20 - Independent Samples T-Test ####

#create dataset
data = cbind(rnorm(50,5,2),rnorm(50,6,2))
colnames(data) = c('Group1','Group2')
#plot data column means
barplot(colMeans(data))
#convert to long data table
library(reshape2)
long_data=melt(data)
colnames(long_data)=c('NA','Group','Values')

#convert group names to numbers
long_data[,2]=as.numeric(long_data[,2])

plot(long_data[,2:3])

#run a t-test on short form data ... not sure why we created long form data above....
ttest_results_short=t.test(data[,1],data[,2]) # default is variance is not equal (runs a Welsh test) 
ttest_results_short=t.test(data[,1],data[,2],var.equal=TRUE) # equal variances 





#### Episode 21 - Repeated Measures T-Test ####
#remove data
rm(long_data)
rm(ttest_results)
rm(ttest_results_short)

data=cbind(rnorm(50,5,2),rnorm(50,6,2)) #create dataset to use
colnames(data) = c('Time 1','Time 2') #change column names
barplot(colMeans(data)) #plot data in two columns

#convert data to long form
library(reshape2)
long_data=melt(data)
colnames(long_data) =c('NA','Time','Values')
#change column 2 of long data to numeric values
long_data[,2]=as.numeric(long_data[,2])

#plot columns 2 and 3
plot(long_data[,2],long_data[,3])

#conduct a t-test between Time 1 and Time 2 data (paired)
ttest_results=t.test(data[,1],data[,2],paired=TRUE)






#### Episode 22 - Correlations ####
#create data set
data=matrix(NA,50,2) #50 rows, 2 columns
data[,1]=runif(50,0,10) #50 random samples between 0 and 10 in column 1
hist(data[,1]) #create histogram of column 1
data[,2]=data[,1]*rnorm(50,2,.5) #create column 2 data based off col 1 multiplied by random numbers with a column mean of 2 and SD of 0.5

# plot the scatter plot w line of best fit, r value
plot(data, pch=20, col="lightblue")
abline(lm(data[,2]~data[,1]),col="red",lwd=3) #regression line through data (y axis first then x axis in code)

correlation=cor(data) #calculate the correlation between the two columns in "data" 
correlation_test=cor.test(data[,1],data[,2]) #is the correlation significant?

text(paste("r:", round(cor(data[,1], data[,2]), 2)), x = 1, y = 25, col="blue") #put the correlation on the graph



#### Episode 23 - Independent Samples One-Way ANOVA ####

#create data frame w ind variable with 3 levels (i.e.,3 diff distributions)
data= cbind(c(rnorm(50,20,4),rnorm(50,30,4),rnorm(50,35,4)),sort(rep(1:3,50)),1:150) #50 samples per level
#turn data variable into a data frame
data=as.data.frame(data)
plot(data[,2],data[,1]) #plot the data 
#name the columns
colnames(data)=c('Scores','Levels','Subject')
#tell the anova that our second column (i.e., Levels) is a factor
data[,2]=as.factor(data[,2])
#tell the anova that our third column (i.e., Subject) is a factor
data[,3]=as.factor(data[,3])

plot(data[,2],data[,1]) #plot the data 

install.packages("ez")
library(ez)

anova_results=ezANOVA(dv=.(Scores),between=.(Levels),wid=.(Subject),detailed=TRUE,type=3,data=data)
anova_results

#Results: F(2,147) = 191.4, p < 0.0001, ges = 0.72


#### Episode 24 - Repeated Measures One-Way ANOVA ####

#clear GE
rm(list = ls()) 

#create a data set
data = cbind(c(rnorm(50,20,4),rnorm(50,30,4),rnorm(50,35,4)),sort(rep(1:3,50)),1:50)
plot(data[,2],data[,1])
#convert variable to data frame
data=as.data.frame(data)
#name columns
colnames(data)=c('Scores','Levels','Subject')
#change levels and subject to factors
data[,2]=as.factor(data[,2])
data[,3]=as.factor(data[,3])

library(ez)
anova_results=ezANOVA(dv=.(Scores),within=.(Levels),wid = .(Subject),detailed=TRUE,type=3,data=data)

anova_results

#ANOVA Results: F(2,98)=243.1, p<0.0001, ges=0.77

#### Episode 25 - Independent Samples Factorial (2x2) ANOVA ####

#clear GE
rm(list = ls()) 

#create a data set
data = cbind(c(rnorm(50,20,4),rnorm(50,25,4),rnorm(50,30,4),rnorm(50,35,4)),sort(rep(1:2,100)),rep(sort(rep(1:2,50)),2),1:200)
#convert variable to data frame
data=as.data.frame(data)
#name columns
colnames(data)=c('Scores','Levels1','Levels2','Subject')
#change levels and subject to factors
data[,2]=as.factor(data[,2])
data[,3]=as.factor(data[,3])
data[,4]=as.factor(data[,4])

library(ez)
anova_results=ezANOVA(dv=.(Scores),between=.(Levels1,Levels2),wid=.(Subject),detailed=TRUE,type=3,data=data)

anova_results

# ANOVA Results: 
#F(1,196)=376.9, p<0.0001, ges=0.66
#F(1,196)=126.1, p<0.0001, ges=0.39
#F(1,196)=0.25, p=0.06, ges=0.001


#### Episode 26 - Repeated Measures Factorial (2x2) ANOVA ####

#clear GE
rm(list = ls()) 

#create a data set
data = cbind(c(rnorm(50,20,4),rnorm(50,25,4),rnorm(50,30,4),rnorm(50,35,4)),sort(rep(1:2,100)),rep(sort(rep(1:2,50)),2),1:50)
#convert variable to data frame
data=as.data.frame(data)
#name columns
colnames(data)=c('Scores','Levels1','Levels2','Subject')
#change levels and subject to factors
data[,2]=as.factor(data[,2])
data[,3]=as.factor(data[,3])
data[,4]=as.factor(data[,4])

library(ez)
anova_results=ezANOVA(dv=.(Scores),within=.(Levels1,Levels2),wid=.(Subject),detailed=TRUE,type=3,data=data)

anova_results

# ANOVA Results: 
#F(1,196)=366.0, p<0.0001, ges=0.63
#F(1,196)=91.2, p<0.0001, ges=0.30
#F(1,196)=0.25, p=0.06, ges=0.001

#### Episode 27 - Mixed Effects Factorial (2x2) ANOVA ####

#clear GE
rm(list = ls()) 

#create a data set
data = cbind(c(rnorm(50,20,4),rnorm(50,25,4),rnorm(50,30,4),rnorm(50,35,4)),sort(rep(1:2,100)),rep(sort(rep(1:2,50)),2),c(rep(1:50,2),rep(51:100,2)))
#convert variable to data frame
data=as.data.frame(data)
#name columns
colnames(data)=c('Scores','Group','Condition','Subject')
#change levels and subject to factors
data[,2]=as.factor(data[,2])
data[,3]=as.factor(data[,3])
data[,4]=as.factor(data[,4])

library(ez)
anova_results=ezANOVA(dv=.(Scores),between=.(Group),within=.(Condition),wid=.(Subject),detailed=TRUE,type=3,data=data)

anova_results #this shows the results in the console

# ANOVA Results: 
#F(1,98)=401.7, p<0.0001, ges=0.64
#F(1,98)=70.2, p<0.0001, ges=0.29
#F(1,98)=0.35, p=0.56, ges=0.002


#### Episode 28 - Simple Linear Regression ####
#clear GE
rm(list = ls()) 

#create data matrix to use
data=matrix(NA,50,2)
data[,1]=runif(50,0,10)
data[,2]=data[,1]*rnorm(50,2,1)
colnames(data)=c('Time1','Time2') #name the columns

plot(data)

#create linear regression (predicting Time2 from Time1)
lr_results=lm(data[,2]~data[,1]) #~ symbol means we are predicting something
lr_results
abline(lr_results)

#check significance with anova
anova(lr_results)

#F(1,48)=92.5, p<0.0001

#### Episode 29 - Multiple Regression ####
#clear GE
rm(list = ls()) 

reg_data=data.frame(
  Scores=c(rnorm(50,100,20),rnorm(50,140,20),rnorm(50,110,20),rnorm(50,200,20)),
  Factor1=c(rep(-1,100),rep(1,100)),
  Factor2=rep(c(rep(-1,50),rep(1,50)),2),
  Subject=1:200
)

#change Factor1 and 2 to factors
reg_data[,2]=as.factor(reg_data[,2])
reg_data[,3]=as.factor(reg_data[,3])
reg_data[,4]=as.factor(reg_data[,4])

#simple regression for fun:
reg_results=lm(Scores~Factor1,data=reg_data)
reg_results
anova(reg_results)

#multiple regression:
reg_results=lm(Scores~Factor1+Factor2+Factor1*Factor2,data=reg_data)
reg_results
anova(reg_results)

#can do the same multiple regression with this:
#multiple regression:
reg_results=lm(Scores~Factor1*Factor2,data=reg_data)
reg_results
anova(reg_results)

#include sum of squares
library(car)
Anova(reg_results,type=3)



