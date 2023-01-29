# Clear the workspace

#import libraries
library(forecast)
library(tseries)
library(timeSeries)
library(ggplot2)
library(readxl)
library(err)
library(Metrics)
#import data from excel
data <- read_excel("swemacroupdated.xlsx")


#Prep data to explore:

data_ts = ts(data$Inflation, start = c(1982,1), frequency = 12)
summary(data_ts)
print(summary(data_ts))
plot(data_ts, xlab = "Time (years)", ylab = "Inflation ( percentage)")

#test for stationarity, k = 0
adf.test(data_ts, alternative="stationary", k=0) 

#The Random walk 
inflatnrate <- data_ts #ts is to make it time series
fit_rm  <- arima(inflatnrate,  order = c(0,1,0))
print(summary(fit_rm))
residuals(fit_rm)
checkresiduals(fit_rm)



rw_loss_SE <- matrix(NA, nrow = length(200:474), ncol = 12)
rw_loss_AE <- matrix(NA, nrow = length(200:474), ncol = 12)


#Rolling forecast and function to calculate MSE and MAE 
for (t in c(200:474)){
  cat("at ", t," forcast from", t+1, " to ",  t+12, "\n" )
  inflation_t <-  inflatnrate[1:t]
  inflation_fore_obs_t <-  inflatnrate[(t+1):(t+12)]
  fit_rw  <- arima(inflation_t,  order = c(0,1,0))
  forecast_rw <- forecast(fit_rw, h = 12)
  rw_loss_SE[t-199,] <-  as.numeric((forecast_rw$mean - inflation_fore_obs_t)^2)
  rw_loss_AE[t-199,] <-  as.numeric(abs(forecast_rw$mean - inflation_fore_obs_t))
}

###############################################################
#forecast_rw$mean  #mean forecast for random walk
#as.numeric((forecast_rw$mean - inflation_fore_obs_t)^2)#loss functn random wlk
#as.numeric((forecast_rw$mean - inflation_fore_obs_t))
#as.numeric(mae(forecast_rw$mean, inflation_fore_obs_t))mae functn not useful here


colMeans (rw_loss_SE) #MSE random walk
colMeans(rw_loss_AE) # MAE random walk
plot(colMeans(rw_loss_SE))
plot(colMeans(rw_loss_SE))

#Here I want a grapgh that can show observe and forecast
##################################################

#################################################

#AR  model using auto.arima 

#output suggests arima(1,1,1), arima(2,0,2) howewver Im intrested 
#in AR(1) (1,0,0) hence the restriction
data <- auto.arima(data_ts[1:200], d = 0, D = 0, max.P = 0, max.Q = 0)
data <- auto.arima(data_ts[1:400], d = 0, D = 0, max.P = 0, max.Q = 0)
summary(data)
#model automatic selection ARIMAARIMA(1,0,0)

ar_loss <- matrix(NA, nrow = length(200:474), ncol = 12)

ar_loss_SE <- matrix(NA, nrow = length(200:474), ncol = 12)
ar_loss_AE <- matrix(NA, nrow = length(200:474), ncol = 12)


#Rolling forecast for 12 months ahead with MSE MAE
for (t in c(200:474)){
  cat("at ", t," forcast from", t+1, " to ",  t+12, "\n" )
  inflation_t <-  inflatnrate[1:t]
  inflation_fore_obs_t <-  inflatnrate[(t+1):(t+12)]
  fit_ar  <- arima(inflation_t,  order = c(1,0,0))
  forecast_ar <- forecast(fit_ar, h = 12)
  ar_loss_SE[t-199,] <-as.numeric((forecast_ar$mean - inflation_fore_obs_t)^2)
  ar_loss_AE[t-199,] <-as.numeric(abs(forecast_ar$mean - inflation_fore_obs_t))
}

colMeans (ar_loss_SE) #AR MSE 
colMeans(ar_loss_AE)  # AR MAE

plot(colMeans(ar_loss_SE))
plot(colMeans(ar_loss_AE))

plot(colMeans(rw_loss_SE))
plot(colMeans(rw_loss_AE))

cbind(colMeans(rw_loss_SE), colMeans(ar_loss_SE), #RW and AR MSE
      colMeans(rw_loss_SE)- colMeans(ar_loss_SE) )#RW - AR MSE

cbind(colMeans(rw_loss_AE), colMeans(ar_loss_AE), #RW and AR MAE
      colMeans(rw_loss_AE)- colMeans(ar_loss_AE))#RW - AR MAE

# I also need a grapgh for observed and forecast or othe graph 
# that can tell the story
#Now I need to do test satistics for p values for
#statistical significance. Example of the table can be seen in 
#table 3 of the article attached to this mail.
      

#check residual for ar
inflatnrate <- data_ts #ts is to make it time series
fit_ar  <- arima(inflatnrate,  order = c(1,0,0))
print(summary(fit_ar))
residuals(fit_ar)
checkresiduals(fit_ar)
 residuals(fit_ar)
plot(residuals(fit_ar))

###################################################

##################################################################

########################################################



