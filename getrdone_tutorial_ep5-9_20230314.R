## Chad Williams Get R Done tutorial
# https://www.youtube.com/watch?v=pJOhYPBYn5U&list=PLmNgrNF3pZqg2GPmcIAWDa1A2-cAfbX37&index=6


#### Episode 5 ####

#variable
a=1

#matrix
data=matrix(1,4,5)

#matrix w 20 random numbers in the matrix
data[]=runif(20,min=0, max=100)

#matrix w 20 random numbers in the matrix
data[1,]=runif(5,min=0, max=100)

#this shows the matrix
View(data)

#create column names of the matrix
colnames(data)=c('Trial_1','Trial_2','Trial_3','Trial_4','Trial_5')
rownames(data)=c('R1','R2','R3','R4')

#multiple the data matrix to make a new matrix
data_2=data*2
View(data_2)

#### Episode 6 - Selecting columns in a matrix ####

#create a data frame
data= data.frame(
  col_1 = c(1,2,3,4,5),
  col_2 = c(2,4,6,8,10)
)

#see data in only one column 
data$col_2

#### Episode 7 - How save a csv file ####

# make a matrix
data_m = matrix(NA,10,3) #no numbers in this yet (NaN's)
# put random numbers in the first column between 1 and 2
data_m[,1]=runif(10,1,2)
data_m[,2]=runif(10,2,3)
data_m[,3]=runif(10,3,4)

#save a csv
write.csv(data_m,'matrix_example.csv')
#to check to see where the csv is stored, check working directory
getwd()

#### Episode 8 - how to load a csv file ####

# use this to clear the global environment or click the broom icon
rm(list = ls())

#set the working directory to where the csv file is
setwd("C:/Users/Dal User/Documents")
#read csv
data=read.csv('matrix_example.csv')

#that code adds the row names as a column, take column to put in row names
data=read.csv('matrix_example.csv',row.names = 1)

#### Episode 9 - Functions ####

#create a function
square_my_number=function(my_number){
  squared_number = my_number^2
  return(squared_number)
}
#run the function
square_my_number(8)

#try a new function
power_my_number=function(my_number,powered){
  powered_number = my_number^powered
  return(powered_number)
}
#run the function and save the variable as "a" with the result
a=power_my_number(3,3)

#read a script with a function (strings)
read_my_script=function(first_word,second_word){
  combined_words=c(first_word,second_word)
  print(combined_words)
  return(combined_words)
}
#run the script function (strings)
my_name=read_my_script('Josh','Goreham')
