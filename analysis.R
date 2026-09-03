# 1
# Load necessary libraries
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
library(seasonal)
library(dplyr)
library(tidyr)
library(TTR)
library(zoo) 
library(astsa)

# Read the data 
coffee_data <- read.csv("data/coffee_imports_pt.csv")

# Convert to time series
coffee_ts <- ts(coffee_data$Quantity, frequency=12, start=c(2005,1))

# Plot the time series
autoplot(coffee_ts) + 
  ggtitle("Monthly Coffee Imports in Portugal (2005-2020)") +
  ylab("Quantity (kg)") +
  xlab("Year")


train <- window(coffee_ts, end=c(2018,12)) 
test <- window(coffee_ts, start=c(2019,1), end=c(2019,12)) 


# 2 Smoothing Methods
# 2.1 Single Moving Average

ma_12 <- rollmean(train, k = 12, align = "center")
autoplot(train, series = "Original") +
  autolayer(ma_12, series = "12-month MA") +
  labs(title = "Original Series vs 12-month Moving Average",
       y = "Quantity", x = "Time") +
  scale_color_manual(values = c("Original" = "black", "12-month MA" = "blue"))

# 2.2 Simple Exponential Smoothing

# Simple exponential smoothing
ses_coffee <- ses(train, h=12)
summary(ses_coffee)
autoplot(ses_coffee) +
  ggtitle("Simple Exponential Smoothing Forecast") +
  ylab("Quantity (kg)")

autoplot(ses_coffee) +
  ggtitle("Simple Exponential Smoothing Forecast") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2018, 2020))

# 2.3 Holt's Linear Trend Method

# Holt's linear trend method
holt_coffee <- holt(train, h=12)
summary(holt_coffee)
autoplot(holt_coffee) +
  ggtitle("Holt's Linear Trend Forecast") +
  ylab("Quantity (kg)")

autoplot(holt_coffee) +
  ggtitle("Holt's Linear Trend Forecast") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2018, 2020))

# 2.4 Holt-Winters Seasonal Method

# Holt-Winters seasonal method
hw_coffee <- hw(train, seasonal="additive", h=12)
summary(hw_coffee)
autoplot(hw_coffee) +
  ggtitle("Holt-Winters Additive Seasonal Forecast") +
  ylab("Quantity (kg)")

autoplot(hw_coffee) +
  ggtitle("Holt-Winters Additive Seasonal Forecast") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2018, 2020))

# Check if multiplicative seasonality might be better
hw_coffee_mult <- hw(train, seasonal="multiplicative", h=12)
summary(hw_coffee_mult)
autoplot(hw_coffee_mult) +
  ggtitle("Holt-Winters Multiplicative Seasonal Forecast") +
  ylab("Quantity (kg)")

autoplot(hw_coffee_mult) +
  ggtitle("Holt-Winters Multiplicative Seasonal Forecast") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2018, 2020))

autoplot(test, series="Original") +
  autolayer(hw_coffee$mean, series="Additive") +
  autolayer(hw_coffee_mult$mean, series="Multiplicative") +
  ggtitle("Additive vs Multiplicative Holt-Winters") +
  ylab("Quantity") +
  guides(colour=guide_legend(title="Model"))

accuracy(hw_coffee, test)
accuracy(hw_coffee_mult, test)

# 3 Decomposition Methods
# 3.1 Classical Decomposition

# Classical decomposition
decomp_add <- decompose(train, type="additive")
autoplot(decomp_add) + ggtitle("Classical Additive Decomposition")

summary(decomp_add$trend)
summary(decomp_add$seasonal)
summary(decomp_add$random)

last_trend_add <- tail(na.omit(decomp_add$trend), 1)
seasonal_pattern_add <- decomp_add$seasonal[1:12]

# Build forecast
forecast_add_decomp <- ts(rep(last_trend_add, 12) + seasonal_pattern_add, 
                          start = c(2019, 1), frequency = 12)

autoplot(test, series="Actual") +
  autolayer(forecast_add_decomp, series="Forecast (Classical Add)") +
  ggtitle("Forecast from Classical Additive Decomposition")

# Trend + Seasonal + Residual components
add_trend_forecast <- ts(rep(last_trend_add, 12), start = c(2019, 1), frequency = 12)
add_seasonal_forecast <- ts(seasonal_pattern_add, start = c(2019, 1), frequency = 12)
add_random_forecast <- forecast_add_decomp - add_trend_forecast - add_seasonal_forecast

forecast_add_decomp
add_trend_forecast
add_seasonal_forecast
add_random_forecast

# Multiplicative Decomposition
decomp_mult <- decompose(train, type="multiplicative")
autoplot(decomp_mult) + ggtitle("Classical Multiplicative Decomposition")

summary(decomp_mult$trend)
summary(decomp_mult$seasonal)
summary(decomp_mult$random)

last_trend_mult <- tail(na.omit(decomp_mult$trend), 1)
seasonal_pattern_mult <- decomp_mult$seasonal[1:12]

trend_forecast_mult <- rep(last_trend_mult, 12)

forecast_mult_decomp <- ts(trend_forecast_mult * seasonal_pattern_mult,
                           start = c(2019, 1), frequency = 12)

autoplot(test, series="Actual") +
  autolayer(forecast_mult_decomp, series="Forecast (Classical Mult)") +
  ggtitle("Forecast from Classical Multiplicative Decomposition")

# Components
mult_trend_forecast <- ts(rep(last_trend_mult, 12), start = c(2019, 1), frequency = 12)
mult_seasonal_forecast <- ts(seasonal_pattern_mult, start = c(2019, 1), frequency = 12)
mult_random_forecast <- forecast_mult_decomp / (mult_trend_forecast * mult_seasonal_forecast)

forecast_mult_decomp
mult_trend_forecast 
mult_seasonal_forecast 
mult_random_forecast

# Seasonally adjusted data
sa_add <- train - decomp_add$seasonal
sa_mult <- train / decomp_mult$seasonal

autoplot(cbind(Original=train, 
               `Additive Seasonally Adjusted`=sa_add,
               `Multiplicative Seasonally Adjusted`=sa_mult)) +
  ggtitle("Original vs Seasonally Adjusted Series")

autoplot(ts(cbind(Original = train,
                  Additive_Adjusted = sa_add),
            start = start(train), frequency = 12)) +
  ggtitle("Original vs Seasonally Adjusted Series") +
  ylab("Quantity")

autoplot(ts(cbind(Original = train,
                  Multiplicative_Adjusted = sa_mult),
            start = start(train), frequency = 12)) +
  ggtitle("Original vs Seasonally Adjusted Series") +
  ylab("Quantity")

#autoplot(decomp_add$trend) + ggtitle("Trend-Cycle (Additive)")
#autoplot(decomp_add$seasonal) + ggtitle("Seasonal (Additive)")
#autoplot(decomp_add$random) + ggtitle("Residual/Error (Additive)")

# 3.2 STL Decomposition

# STL decomposition
stl_coffee <- stl(train, s.window="periodic")
autoplot(stl_coffee) + ggtitle("STL Decomposition")

head(stl_coffee$time.series)
summary(stl_coffee$time.series)

stl_seasonal <- stl_coffee$time.series[, "seasonal"]
stl_trend <- stl_coffee$time.series[, "trend"]

# Forecast trend with naive
last_trend_stl <- tail(na.omit(stl_coffee$time.series[, "trend"]), 1)
trend_forecast_stl <- ts(rep(last_trend_stl, 12), start = c(2019, 1), frequency = 12)

# Seasonal pattern: use the last year of seasonal values
seasonal_pattern_stl <- ts(tail(stl_coffee$time.series[, "seasonal"], 12),
                           start = c(2019, 1), frequency = 12)

# Combine to create the full forecast
forecast_stl <- ts(trend_forecast_stl + seasonal_pattern_stl,
                   start = c(2019, 1), frequency = 12)

autoplot(test, series = "Actual") +
  autolayer(forecast_stl, series = "Forecast (STL)") +
  ggtitle("Forecast from STL Decomposition") +
  ylab("Quantity") +
  guides(colour = guide_legend(title = "Series"))

# Components
stl_trend_forecast <- trend_forecast_stl
stl_seasonal_forecast <- ts(seasonal_pattern_stl, start=c(2019,1), frequency=12)
stl_random_forecast <- forecast_stl - stl_trend_forecast - stl_seasonal_forecast

forecast_stl
stl_trend_forecast 
stl_seasonal_forecast 
stl_random_forecast

# Seasonally adjusted from STL
sa_stl <- seasadj(stl_coffee)
autoplot(cbind(Original=train, `STL Seasonally Adjusted`=sa_stl)) +
  ggtitle("Original vs STL Seasonally Adjusted Series")

# 3.4 Comparing the Methods

accuracy(forecast_add_decomp,test)
accuracy(forecast_mult_decomp,test)
accuracy(forecast_stl,test)

# 4 SARIMA Modeling
# 4.1 Data Preparation and Stationarity

# Differencing Needs
nsdiffs(train)
ndiffs(train) 

# Create differenced series
diff_train <- diff(train, differences = 1)

# Stationarity
adf.test(diff_train)  
kpss.test(diff_train) 

autoplot(diff_train) + 
  ggtitle("First Differenced Series") +
  ylab("Differenced Quantity")

ggAcf(diff_train) + ggtitle("ACF of Differenced Series")
ggPacf(diff_train) + ggtitle("PACF of Differenced Series")

# 4.2 Model Selection and Estimation

# Automated SARIMA selection
#best_parameters <- auto.arima(train, d = 1, D = 0, stepwise=FALSE, approximation=FALSE)
#summary(best_parameters)

# Manual SARIMA based on ACF/PACF
sarima1 <- sarima(train,2,1,1,1,0,0,12)
sarima2 <- sarima(train,1,1,1,1,0,0,12) 
sarima3 <- sarima(train,2,1,1,0,0,1,12)
sarima4 <- sarima(train,2,1,1,0,0,2,12)

# Check residuals
#checkresiduals(sarima1)
#Box.test(residuals(sarima1), type = "Ljung-Box", lag = 12)  
#Box.test(residuals(sarima2), type = "Ljung-Box", lag = 12) 
#Box.test(residuals(sarima3), type = "Ljung-Box", lag = 12) 
#Box.test(residuals(sarima4), type = "Ljung-Box", lag = 12) 

# 4.3 Model Validation

p <- 2
d <- 1
q <- 1
P <- 1
D <- 0
Q <- 0
S <- 12

# Fit model to training data 
model_train <- Arima(train, order = c(p, d, q),
                     seasonal = list(order = c(P, D, Q), period = S))

shapiro.test(residuals(model_train)) 

best_model <- model_train

# Forecast and compare with test set
forecast_train <- forecast(model_train, h = length(test))
accuracy(forecast_train, test)

forecast_train

# Plot forecast vs actual
autoplot(forecast_train) + 
  ggtitle("SARIMA Forecast vs Actual") +
  ylab("Quantity (kg)") + 
  coord_cartesian(xlim = c(2018, 2020))


# 5. Forecast Comparison

# Generate forecasts from all methods
h <- 12 # Forecast horizon
fc_ses <- ses(train, h=h)
fc_holt <- holt(train, h=h)
fc_hw_add <- hw(train, seasonal="additive", h=h)
fc_hw_mult <- hw(train, seasonal="multiplicative", h=h)
fc_sarima <- forecast(best_model, h=h)

accuracy(fc_ses, test)
accuracy(fc_holt, test)
accuracy(fc_hw_add, test)
accuracy(fc_hw_mult, test)
accuracy(fc_sarima, test)

# Plot comparison
autoplot(train) +
  autolayer(fc_ses, series="SES", PI=FALSE) +
  autolayer(fc_holt, series="Holt", PI=FALSE) +
  autolayer(fc_hw_add, series="HW Add", PI=FALSE) +
  autolayer(fc_hw_mult, series="HW Mult", PI=FALSE) +
  autolayer(fc_sarima, series="SARIMA", PI=FALSE) +
  ggtitle("Forecast Comparison") +
  ylab("Quantity (kg)") +
  guides(colour=guide_legend(title="Method")) + coord_cartesian(xlim = c(2018, 2020))

# 6. Final Forecasts with Prediction Intervals

# Generate final forecasts with 95% prediction intervals
final_forecast <- forecast(best_model, h=12, level=95)
autoplot(final_forecast) +
  ggtitle("Final SARIMA Forecast with 95% Prediction Intervals") +
  ylab("Quantity (kg)") + coord_cartesian(xlim = c(2018, 2020))

# autoplot(window(train, start=c(2016))) + 
#   autolayer(final_forecast, series="2019 Forecast") + 
#   ggtitle("Forecast for 2019 vs Actual") +
#   ylab("Quantity (kg)")

get_metrics <- function(forecast_obj, test_data) {
  acc <- accuracy(forecast_obj, test_data)
  data.frame(
    ME   = round(acc["Test set", "ME"], 2),
    RMSE = round(acc["Test set", "RMSE"], 2),
    MAE  = round(acc["Test set", "MAE"], 2),
    MPE  = round(acc["Test set", "MPE"], 2),
    MAPE = round(acc["Test set", "MAPE"], 2)
  )
}

# Compute metrics for each model
smoothing_metrics   <- get_metrics(fc_hw_add, test)
decomp_metrics      <- get_metrics(forecast_add_decomp, test)
sarima_metrics      <- get_metrics(forecast_train, test)

# Combine into one table
comparison_table <- rbind(
  Smoothing     = smoothing_metrics,
  Decomposition = decomp_metrics,
  SARIMA        = sarima_metrics
)

# Show the comparison table
print(comparison_table)

autoplot(train) + 
  autolayer(fc_hw_add, series="HW Add", PI = FALSE) +
  autolayer(forecast_add_decomp, series="Add Decomp") +
  autolayer(forecast_train, series="SARIMA", PI = FALSE) +
  autolayer(test, series = "Actual") +
  ylab("Quantity (kg)") + 
  coord_cartesian(xlim = c(2018, 2020))


autoplot(forecast_train) + 
  autolayer(test, series = "Actual", color = "red") +
  coord_cartesian(xlim = c(2018, 2020)) +
  labs(
    title = "SARIMA(2,1,1)(1,0,0)[12] Forecast vs 2019 Holdout",
    x = "Time",
    y = "Quantity (kg)"
  ) +
  theme_minimal()


