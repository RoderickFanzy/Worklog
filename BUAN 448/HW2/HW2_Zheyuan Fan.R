### BUAN 448 Homework 1

### Zheyuan (Roderick) Fan
### Sept 4th 2022

## Using the dataset “ToyotaCorolla.csv”, complete the following tasks: 

## Clean Working Environment
rm(list = ls(all=TRUE))

#1 Set a working directory and read the data file. (Please show the command)

setwd("D:/A_Lehigh/2022 Fall/BUAN 488 - Predictive Analytics")
toyota <- read.csv("ToyotaCorolla.csv")

#2 Remove observations (i.e, rows) from 3-200, 500-502, and 1000-1205.

toyota <- toyota[-c(3:200,500:502,1000:1205),]


#3 Remove observations if KM>=120,000. How many records did you remove?


toyota <- subset(toyota, toyota$KM<120000)
t(dim(toyota[toyota$KM>=120000,]))

###39 records have been removed###


#4 Remove variable Met_Color.

toyota <- subset(toyota, select = -c(Met_Color))

#5 Replace any value greater than 100 in HP with 180.

toyota$HP[toyota$HP>100] <- 180



#6 Create a new variable called "Old_Car". If the Age_08_04>=12, it takes value of "Old", otherwise "Not too old". 
# (Hint: Use ifelse statement)

toyota$Old_car <- ifelse(toyota$Age_08_04>=12,"Old","Not too old")

table(toyota$Old_car)

#7 Generate dummy variables for Fuel_Type. 
# (Hint: Use ifelse statement and Create two new columns, Diesel & Petrol, with binary values) 

toyota$Diesel <- ifelse(toyota$Fuel_Type=="Diesel",1,0)
toyota$Petrol <- ifelse(toyota$Fuel_Type=="Petrol",1,0)
