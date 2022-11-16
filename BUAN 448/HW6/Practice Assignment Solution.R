
##Problem Predicting Airfare on New Routes.
##The following problem takes place in the United States in the late 1990s, when
##many major US cities were facing issues with airport congestion, partly as a 
##result of the 1978 deregulation of airlines. Both fares and routes were freed
##from regulation, and low-fare carriers such as Southwest (SW) began competing 
##on existing routes and starting nonstop service on routes that previously 
##lacked it. Building completely new airports is generally not feasible, but 
##sometimes decommissioned military bases or smaller municipal airports can be
##reconfigured as regional or larger commercial airports. There are numerous 
##players and interests involved in the issue (airlines, city, state and 
##federal authorities, civic groups, the military, airport operators), and an
##aviation consulting firm is seeking advisory contracts with these players. 
##The firm needs predictive models to support its consulting service. One thing
##the firm might want to be able to predict is fares, in the event a new airport
##is brought into service. The firm starts with the file Airfares.csv, which 
##contains real data that were collected between Q3-1996 and Q2-1997. The 
##variables in these data are listed in Table 6.11, and are believed to be 
##important in predicting FARE. Some airport-to-airport data are available, 
##but most data are at the city-to-city level. One question that will be of 
##interest in the analysis is the effect that the presence or absence of 
#Southwest has on FARE.

##TABLE 6.11 DESCRIPTION OF VARIABLES FOR AIRFARE EXAMPLE
##S_CODE   Starting airport's code
##S_CITY   Starting city
##E_CODE   Ending airport's code
##E_CITY   Ending city
##COUPON   Average number of coupons (a one-coupon flight is a nonstop flight, 
##         a two-coupon flight is a one-stop flight, etc.) for that route
##NEW      Number of new carriers entering that route between Q3-96 and Q2-97
##VACATION Whether (Yes) or not (No) a vacation route
##SW       Whether (Yes) or not (No) Southwest Airlines serves that route
##HI       Herfindahl index: measure of market concentration
##S_INCOME Starting city's average personal income
##E_INCOME Ending city's average personal income
##S_POP    Starting city's population
##E_POP    Ending city's population
##SLOT     Whether or not either endpoint airport is slot-controlled
##         (this is a measure of airport congestion)
##GATE     Whether or not either endpoint airport has gate constraints
##         (this is another measure of airport congestion)
##DISTANCE Distance between two endpoint airports in miles
##PAX      Number of passengers on that route during period of data collection
##FARE     Average fare on that route

# ==================== HW4 Solution ========
# Load the data.
airfares.df <- read.csv("Airfares.csv")
#remove unneccessary variable
airfares.df <- airfares.df[,-c(1:4)]
# Which variables are numeric?
str(airfares.df)
summary(airfares.df)


##6.3.a. Explore the numerical predictors and response (FARE) by creating a 
##correlation table and examining some scatterplots between FARE and those 
##predictors. What seems to be the best single predictor of FARE?

t(t(names(airfares.df)))
#correlation matrix of numeric variables
cor(airfares.df[, c(1, 2, 5:9, 12:14)])
plot(airfares.df$DISTANCE, airfares.df$FARE)
plot(airfares.df$COUPON, airfares.df$FARE)

#DISTANCE is the best single predictor of FARE.

##6.3.b. Explore the categorical predictors (excluding the first four) by 
##computing the percentage of flights in each category. Create a pivot table 
##with the average fare in each category. Which categorical predictor seems 
##best for predicting FARE?

# ======================== Not a good idea ====================================
# By default R converts binary categorical variables to factors and encode categories to 
# 1 and 2 instead of 0 and 1. We replace the binary categorical variables with 
# dummy variables.
# These predictors would still be considered numeric but reference cateogry would
# be the one which is set as zero.
airfares.df$VACATION <- ifelse(airfares.df$VACATION == "Yes", 1 , 0)  # Create a dummy variable.
airfares.df$SW <- ifelse(airfares.df$SW == "Yes", 1 , 0)  # Create a dummy variable.
airfares.df$SLOT <- ifelse(airfares.df$SLOT == "Controlled", 1 , 0)  # Create a dummy variable.
airfares.df$GATE <- ifelse(airfares.df$GATE == "Constrained", 1 , 0)  # Create a dummy variable.
str(airfares.df)
# =================================



par(mfcol=c(1,4))
#pivot tables for % of flights in each category
freq.Vacation <- table(airfares.df$VACATION)
round(prop.table(freq.Vacation),4)*100
airfares.df$VACATION <- as.factor(airfares.df$VACATION)
plot(airfares.df$VACATION, airfares.df$FARE, main = "VACATION")
#> round(prop.table(freq.Vacation),4)*100
#    0     1 
#73.35 26.65

freq.sw <- table(airfares.df$SW)
round(prop.table(freq.sw),4)*100
airfares.df$SW <- as.factor(airfares.df$SW)
plot(airfares.df$SW, airfares.df$FARE, main = "SW")
#> round(prop.table(freq.sw),4)*100
#    0     1 
#69.59 30.41

freq.slot <- table(airfares.df$SLOT)
round(prop.table(freq.slot),4)*100
airfares.df$SLOT <- as.factor(airfares.df$SLOT)
plot(airfares.df$SLOT, airfares.df$FARE, main = "SLOT")
#> round(prop.table(freq.slot),4)*100
#    0     1 
#71.47 28.53 

freq.gate <- table(airfares.df$GATE)
round(prop.table(freq.gate),4)*100
airfares.df$GATE <- as.factor(airfares.df$GATE)
plot(airfares.df$GATE, airfares.df$FARE, main = "GATE")
#> round(prop.table(freq.gate),4)*100
#
#    0     1 
#80.56 19.44

#pivot tables for average fare in each category
aggregate(airfares.df$FARE, list(airfares.df$VACATION), mean)
#> aggregate(airfares.df$FARE, list(airfares.df$VACATION), mean)
#  Group.1        x
#1       0 173.5525
#2       1 125.9809
aggregate(airfares.df$FARE, list(airfares.df$SW), mean)
#> aggregate(airfares.df$FARE, list(airfares.df$SW), mean)
#  Group.1         x
#1       0 188.18279
#2       1  98.38227
aggregate(airfares.df$FARE, list(airfares.df$SLOT), mean)
#> aggregate(airfares.df$FARE, list(airfares.df$SLOT), mean)
#  Group.1        x
#1       0 150.8257
#2       1 186.0594
aggregate(airfares.df$FARE, list(airfares.df$GATE), mean)
#> aggregate(airfares.df$FARE, list(airfares.df$GATE), mean)
#   Group.1       x
# 1       0 153.096
# 2       1 193.129

#SW is the single best categorical predictor of FARE.

##6.3.c. Find a model for predicting the average fare on a new route:

# Partition the data into training and testing sets.
ntotal <- length(airfares.df$FARE)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
set.seed(202)  # Seed of random number generator for reproducibility.
ntrain.index <- sort(sample(ntotal, ntrain))  # Sample rows randomly.
train.df <- airfares.df[ntrain.index, ]
valid.df <- airfares.df[-ntrain.index, ]

reg <- lm(FARE ~ ., data = train.df)
summary(reg)


# validation
library(forecast) #for accuracy measures
pred.valid <- predict(reg, valid.df)
#Comparison
accuracy(pred.valid, valid.df$FARE)
accuracy(reg$fitted.values, valid.df$FARE)

# ============================= HW5 solution ==============
# Load the data.
airfares.df <- read.csv("Airfares.csv")
#remove unneccessary variable
airfares.df <- airfares.df[,-c(1:4)]

# Partition the data into training and testing sets.
ntotal <- length(airfares.df$FARE)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
set.seed(202)  # Seed of random number generator for reproducibility.
ntrain.index <- sort(sample(ntotal, ntrain))  # Sample rows randomly.
train.df <- airfares.df[ntrain.index, ]
valid.df <- airfares.df[-ntrain.index, ]

# create model with no predictors
air.lm.null <- lm(FARE ~ 1, data = train.df)
summary(air.lm.null)
# create model with all predictors
air.lm.full <- lm(FARE ~., data = train.df)
summary(air.lm.full)

# use step() to run forward regression.
air.lm.step.forward <- step(air.lm.null, scope=list(lower=air.lm.null, upper=air.lm.full), direction = "forward")
summary(air.lm.step.forward)  # Which variables were added?
air.lm.step.pred.forward <- predict(air.lm.step.forward, valid.df)
library(forecast)
forward.acc <- accuracy(air.lm.step.pred.forward, valid.df$FARE)

# use step() to run stepwise regression.
air.lm.step.backward <- step(air.lm.full, direction = "backward")
summary(air.lm.step.backward)  # Which variables were dropped?
air.lm.step.pred.backward <- predict(air.lm.step.backward, valid.df)
back.acc <- accuracy(air.lm.step.pred.backward, valid.df$FARE)

# use step() to run stepwise regression.
air.lm.step <- step(air.lm.full, direction = "both")
summary(air.lm.step)  # Which variables were dropped/added?
air.lm.step.pred <- predict(air.lm.step, valid.df)
both.acc <- accuracy(air.lm.step.pred, valid.df$FARE)

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
search <- regsubsets(FARE ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
# show metric
t(t(adjr2))
sum$which[10,]
sum$which[11,]
