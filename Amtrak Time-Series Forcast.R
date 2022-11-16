rm(list = ls(all=T))

library(here)
library(forecast)
amtrak <- read.csv(here("Amtrak.csv"))

head(amtrak)
tail(amtrak)
summary(amtrak$Ridership)

amtrak.ts <- ts(amtrak$Ridership,
                start = c(1991, 1), end = c(2004, 3), freq = 12)

# plot the time series
plot(amtrak.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

# Partition data
nValid <- 36
nTrain <- length(amtrak.ts) - nValid
train.ts <- window(amtrak.ts, start = c(1991, 1), 
                   end  = c(1991, nTrain))
valid.ts <- window(amtrak.ts, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))

# Fit a linear trend on training data
train.lm <- tslm(train.ts ~ trend )
summary(train.lm)

# Predict validation data
train.lm.pred <- forecast(train.lm, h = nValid) 

# Plot predictions vs actual
plot(amtrak.ts, ylab="Ridership in 000s", ylim=c(1300, 2400),main="Linear Fit")
lines(train.lm$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.pred$mean, col=3, lty=3, lwd=2)
legend("top", legend=c("Actual","Prediction-Training","Forecast-Validation" ), col=c(1:3),
       lty=c(1,2,3), lwd=c(1,2,2))

# fit exponential trend 
train.lm.expo <- tslm(train.ts ~ trend, lambda = 0)

# Predict for validation data
train.lm.expo.pred <- forecast(train.lm.expo, h = nValid)

# Plot predictions vs actual
plot(amtrak.ts, ylab="Ridership in 000s", ylim=c(1300, 2400))

# add linear fit
lines(train.lm$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.pred$mean, col=2, lty=2, lwd=2)

# add exponential fit
lines(train.lm.expo$fitted.values, col=3, lty=3, lwd=2 )
lines(train.lm.expo.pred$mean, col=3, lty=3, lwd=2)

# add a line to separate validation from training
abline(v=2001.25, col="grey")
text(1998, 2400, "Training")
text(2003, 2400, "Validation")

# legend
legend("topleft", legend=c("Actual","Linear","Exponential" ), col=c(1:3), lty=c(1,2,3), lwd=c(1,2,2))



# Polynomial trend
# fit quadratic trend using function I(), which treats an object "as is".
train.lm.poly <- tslm(train.ts ~ trend + I(trend^2) )
summary(train.lm.poly)

# Forecast for validation data
train.lm.poly.pred <- forecast(train.lm.poly, h = nValid)

# Plot predictions vs actual
plot(amtrak.ts, ylab="Ridership in 000s", ylim=c(1300, 2400),main="Quadratic Fit")
lines(train.lm.poly$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.poly.pred$mean, col=3, lty=3, lwd=2)
legend("top", legend=c("Actual","Prediction-Training","Forecast-Validation" ), col=c(1:3),
       lty=c(1,2,3), lwd=c(1,2,2))


# Polynomial trend with seasonality
# include season as a predictor in tslm(), it creastes 11 dummies for season
train.lm.poly.season <- tslm(train.ts ~ trend + I(trend^2) + season)
options(scipen=999)
summary(train.lm.poly.season)

# Forecast for valiaiton data
train.lm.poly.season.pred = forecast(train.lm.poly.season, h=nValid)

# Plot predictions vs actual
plot(amtrak.ts, ylab="Ridership in 000s", ylim=c(1300, 2400))
lines(train.lm.poly.season$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.poly.season.pred$mean, col=3, lty=3, lwd=2)
legend("top", legend=c("Actual","Prediction-Training","Forecast-Validation" ), col=c(1:3),
       lty=c(1,2,3), lwd=c(1,2,2))


#Test for Accuracy
accuracy(train.lm.pred, valid.ts)
accuracy(train.lm.expo.pred, valid.ts)
accuracy(train.lm.poly.pred, valid.ts)
accuracy(train.lm.poly.season.pred, valid.ts)

