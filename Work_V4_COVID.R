# 1
# Load necessary libraries
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
library(seasonal)
library(dplyr)
library(tidyr)
library(tsoutliers)

# Read the data (assuming it's in the R2 sheet)
coffee_data <- read_excel("C:/Users/Hp/Desktop/TS_Project/Coffee.xlsx", sheet = "R") 

# Convert to time series object
# The data is monthly from Jan 2005 to Dec 2024
coffee_ts <- ts(coffee_data$Quantity, frequency=12, start=c(2005,1))

# Plot the time series
autoplot(coffee_ts) + 
  ggtitle("Monthly Coffee Imports in Portugal (2005-2024)") +
  ylab("Quantity (kg)") +
  xlab("Year")


train <- window(coffee_ts, end=c(2023,12)) 
test <- window(coffee_ts, start=c(2024,1), end=c(2024,12)) 

# Detect outliers (e.g., COVID drops)
outliers <- tsoutliers::tso(train, types = c("AO", "TC", "LS"))
plot(outliers)  # Visualize detected outliers

# Extract outlier timestamps
outlier_dates <- outliers$outliers$time

# Create dummy variables for outliers
outlier_dummy <- ts(0, start = start(train), end = end(train), freq = 12)
outlier_dummy[outlier_dates] <- 1

# 2 Smoothing Methods
# 2.1 Simple Exponential Smoothing

# Simple exponential smoothing
ses_coffee <- ses(train, h=12)
summary(ses_coffee)
autoplot(ses_coffee) +
  ggtitle("Simple Exponential Smoothing Forecast") +
  ylab("Quantity (kg)")

# 2.2 Holt's Linear Trend Method

# Holt's linear trend method
holt_coffee <- holt(train, h=12)
summary(holt_coffee)
autoplot(holt_coffee) +
  ggtitle("Holt's Linear Trend Forecast") +
  ylab("Quantity (kg)")

# 2.3 Holt-Winters Seasonal Method

# Holt-Winters seasonal method
hw_coffee <- hw(train, seasonal="additive", h=12)
summary(hw_coffee)
autoplot(hw_coffee) +
  ggtitle("Holt-Winters Additive Seasonal Forecast") +
  ylab("Quantity (kg)")

# Check if multiplicative seasonality might be better
hw_coffee_mult <- hw(train, seasonal="multiplicative", h=12)
summary(hw_coffee_mult)
autoplot(hw_coffee_mult) +
  ggtitle("Holt-Winters Multiplicative Seasonal Forecast") +
  ylab("Quantity (kg)")

# 3 Decomposition Methods
# 3.1 Classical Decomposition

# Classical decomposition
decomp_add <- decompose(train, type="additive")
autoplot(decomp_add) + ggtitle("Classical Additive Decomposition")

decomp_mult <- decompose(train, type="multiplicative")
autoplot(decomp_mult) + ggtitle("Classical Multiplicative Decomposition")

# Seasonally adjusted data
sa_add <- train - decomp_add$seasonal
sa_mult <- train / decomp_mult$seasonal

autoplot(cbind(Original=train, 
               `Additive Seasonally Adjusted`=sa_add,
               `Multiplicative Seasonally Adjusted`=sa_mult)) +
  ggtitle("Original vs Seasonally Adjusted Series")

# 3.2 STL Decomposition

# STL decomposition
stl_coffee <- stl(train, s.window="periodic")
autoplot(stl_coffee) + ggtitle("STL Decomposition")

# Seasonally adjusted from STL
sa_stl <- seasadj(stl_coffee)
autoplot(cbind(Original=train, `STL Seasonally Adjusted`=sa_stl)) +
  ggtitle("Original vs STL Seasonally Adjusted Series")

# 4 SARIMA Modeling
# 4.1 Data Preparation and Stationarity

# Check for stationarity
adf.test(train)  # Augmented Dickey-Fuller test
kpss.test(train) # KPSS test

# Original series is likely not stationary - needs differencing
ndiffs(train) # Number of regular differences needed
nsdiffs(train) # Number of seasonal differences needed

# Create differenced series
diff_coffee <- diff(train)
diff_seasonal <- diff(train, lag=12)
diff_both <- diff(diff(train), lag=12)

# Plot differenced series
autoplot(diff_coffee) + ggtitle("Regularly Differenced Series")
autoplot(diff_seasonal) + ggtitle("Seasonally Differenced Series")
autoplot(diff_both) + ggtitle("Regular and Seasonal Differenced Series")

# Check ACF/PACF of differenced series
ggAcf(diff_both) + ggtitle("ACF of Differenced Series")
ggPacf(diff_both) + ggtitle("PACF of Differenced Series")

# 4.2 Model Selection and Estimation

# Automated SARIMA selection
auto_arima <- auto.arima(train, stepwise=FALSE, approximation=FALSE, xreg = outlier_dummy)
summary(auto_arima)

# Check residuals
checkresiduals(auto_arima)

# Manual SARIMA based on ACF/PACF
# Try a few models and compare AIC
arima1 <- Arima(train, order=c(1,1,1), seasonal=c(0,1,1))
arima2 <- Arima(train, order=c(0,1,1), seasonal=c(0,1,1))
arima3 <- Arima(train, order=c(1,1,0), seasonal=c(1,1,0))

# Compare AIC values
AIC(auto_arima, arima1, arima2, arima3)

# Select best model based on AIC and residual diagnostics
best_model <- auto_arima # or choose manually if better

# 4.3 Model Validation

# Create training and test sets
#train <- window(coffee_ts, end=c(2022,12))
#test <- window(coffee_ts, start=c(2023,1))



# Fit model to training data
model_train <- Arima(train, order=best_model$arma[c(1,6,2)], 
                     seasonal=best_model$arma[c(3,7,4)])

# Forecast and compare with test set
forecast_train <- forecast(model_train, h=length(test))
accuracy(forecast_train, test)

autoplot(forecast_train) + 
  autolayer(test, series="Actual") +
  ggtitle("SARIMA Forecast vs Actual") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2023, 2025))

# 5. Forecast Comparison

# Generate forecasts from all methods
h <- 12 # Forecast horizon
fc_ses <- ses(train, h=h)
fc_holt <- holt(train, h=h)
fc_hw_add <- hw(train, seasonal="additive", h=h)
fc_hw_mult <- hw(train, seasonal="multiplicative", h=h)
fc_arima <- forecast(best_model, h=h)

accuracy(fc_ses, test)
accuracy(fc_holt, test)
accuracy(fc_hw_add, test)
accuracy(fc_hw_mult, test)
accuracy(fc_arima, test)

# Plot comparison
autoplot(train) +
  autolayer(fc_ses, series="SES", PI=FALSE) +
  autolayer(fc_holt, series="Holt", PI=FALSE) +
  autolayer(fc_hw_add, series="HW Add", PI=FALSE) +
  autolayer(fc_hw_mult, series="HW Mult", PI=FALSE) +
  autolayer(fc_arima, series="SARIMA", PI=FALSE) +
  ggtitle("Forecast Comparison") +
  ylab("Quantity (kg)") +
  guides(colour=guide_legend(title="Method")) + coord_cartesian(xlim = c(2023, 2025))

# 6. Final Forecasts with Prediction Intervals

# Generate final forecasts with 95% prediction intervals
final_forecast <- forecast(best_model, h=12, level=95)
autoplot(final_forecast) +
  ggtitle("Final SARIMA Forecast with 95% Prediction Intervals") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2023, 2025))

# Table of forecast values
print(final_forecast)
print(test) 
