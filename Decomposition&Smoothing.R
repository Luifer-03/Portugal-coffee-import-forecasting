# ======================
# 1. Load Required Packages
# ======================
library(tidyverse)
library(forecast)
library(lubridate)
library(ggplot2)
library(readxl)
# ======================
# 2. Load and Prepare the Data
# ======================
# Replace with your actual path if needed
coffee_data <- read_excel("C:/Users/Hp/Desktop/TS_Project/Coffee.xlsx", sheet = "R2")

# ======================
# 3. Convert to Time Series Object
# ======================
coffee_ts <- ts(coffee_data$Quantity, frequency=12, start=c(2005,1))

# Split into Train and Test (Test = 2019)
train <- window(coffee_ts, end = c(2018, 12))
test <- window(coffee_ts, start = c(2019, 1))

# ======================
# 4. Smoothing Methods
# ======================

# 4.1 Moving Average (Centered 12-month)
ma_12 <- ma(train, order = 12)

autoplot(train, series = "Original") +
  autolayer(ma_12, series = "12-Month Moving Average", color = "red") +
  labs(title = "Moving Average Smoothing", y = "Import")

# 4.2 Exponential Smoothing (Holt-Winters)
hw_model <- HoltWinters(train)
autoplot(hw_model$fitted[,1], series = "HW Fitted") +
  autolayer(train, series = "Original") +
  labs(title = "Holt-Winters Smoothing")

# ======================
# 5. Decomposition
# ======================
decomp <- stl(train, s.window = "periodic")
plot(decomp)

# Extract components
trend_component <- decomp$time.series[, "trend"]
seasonal_component <- decomp$time.series[, "seasonal"]
residual_component <- decomp$time.series[, "remainder"]

# ======================
# 6. Seasonally Adjusted Data
# ======================
seasonally_adjusted <- seasadj(decomp)

autoplot(train, series = "Original") +
  autolayer(seasonally_adjusted, series = "Seasonally Adjusted", color = "darkgreen") +
  labs(title = "Seasonally Adjusted Series") + coord_cartesian(xlim = c(2005, 2020))

# ======================
# 7. Forecasting (2019)
# ======================
hw_forecast <- forecast(hw_model, h = 12)

# Plot forecast
autoplot(hw_forecast) +
  autolayer(test, series = "Actual 2019", color = "red") +
  labs(title = "Holt-Winters Forecast for 2019")

autoplot(hw_forecast) +
  autolayer(test, series = "Actual 2019", color = "red") +
  labs(title = "Holt-Winters Forecast for 2019") + coord_cartesian(xlim = c(2018, 2020))
# ======================
# 8. Accuracy
# ======================
accuracy(hw_forecast, test)

# ======================
# 9. Summary of Results (For Report)
# ======================
cat("Summary:\n")
cat("- Trend, seasonal, and residual components extracted using STL decomposition.\n")
cat("- Seasonally adjusted series was calculated and plotted.\n")
cat("- 12-month moving average provided smoothed trend.\n")
cat("- Holt-Winters smoothing used for forecasting 2019, compared with actuals.\n")
cat("- Forecast accuracy was assessed using MAPE, RMSE, etc.\n")
