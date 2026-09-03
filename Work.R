# Install if necessary
# install.packages(c("readxl", "forecast", "tseries", "ggplot2", "lubridate"))

library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)

# Read the dataset (assuming sheet "Dataset" and header starts at row 6)
coffee_data <- read_excel("C:/Users/Hp/Desktop/TS_Project/Coffee.xlsx", sheet = "R2")

# Rename columns and create a Date column
colnames(coffee_data) <- c("Year", "Month", "Quantity")
coffee_data$Date <- as.Date(paste(coffee_data$Year, coffee_data$Month, "1", sep = "-"), "%Y-%b-%d")

# Convert to time series object (monthly)
coffee_ts <- ts(coffee_data$Quantity, start = c(coffee_data$Year[1], match(coffee_data$Month[1], month.abb)), frequency = 12)

autoplot(coffee_ts) + ggtitle("Monthly Coffee Imports (KG)") + ylab("KG") + xlab("Year")

################################################################################

# Classical decomposition
decomp_add <- decompose(coffee_ts, type = "additive")
decomp_mult <- decompose(coffee_ts, type = "multiplicative")

autoplot(decomp_add) + ggtitle("Additive Decomposition")
autoplot(decomp_mult) + ggtitle("Multiplicative Decomposition")

# Seasonally adjusted series
sa_series <- seasadj(decompose(coffee_ts))
autoplot(sa_series) + ggtitle("Seasonally Adjusted Series")

# Smoothing - Holt-Winters methods
fit_hw_add <- hw(coffee_ts, seasonal = "additive")
fit_hw_mult <- hw(coffee_ts, seasonal = "multiplicative")

autoplot(fit_hw_add) + ggtitle("Holt-Winters Additive Forecast")
autoplot(fit_hw_mult) + ggtitle("Holt-Winters Multiplicative Forecast")

# Print parameter estimates
fit_hw_add$model$par
fit_hw_mult$model$par

################################################################################

# Check stationarity visually
ggtsdisplay(coffee_ts)

# Apply seasonal differencing (if needed)
ndiffs(coffee_ts)       # Regular differencing order
nsdiffs(coffee_ts)      # Seasonal differencing order

# Fit SARIMA with auto.arima
fit_sarima <- auto.arima(coffee_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

summary(fit_sarima)

# Diagnostic checks
checkresiduals(fit_sarima) # Includes ACF, Ljung-Box

# Manual Ljung-Box (e.g., for lag 24)
Box.test(fit_sarima$residuals, lag = 24, fitdf = length(fit_sarima$coef), type = "Ljung-Box")

################################################################################

# Split into training 
train <- window(coffee_ts, end = c(2018, 12))
test <- window(coffee_ts, start = c(2019, 1))

# Re-fit models on training data
fit_sarima_train <- auto.arima(train, seasonal = TRUE)
forecast_sarima <- forecast(fit_sarima_train, h = length(test))

fit_hw_train <- hw(train, seasonal = "multiplicative")
forecast_hw <- forecast(fit_hw_train, h = length(test))

# Plot forecasts
autoplot(forecast_sarima) + autolayer(test, series = "Actual") + ggtitle("SARIMA Forecast vs Actual")
autoplot(forecast_hw) + autolayer(test, series = "Actual") + ggtitle("HW Forecast vs Actual")

# Accuracy comparison
accuracy(forecast_sarima, test)
accuracy(forecast_hw, test)

################################################################################

# Refit best model on full data and forecast
best_model <- fit_sarima  # or fit_hw_add / fit_hw_mult depending on accuracy
future_forecast <- forecast(best_model, h = 24)

autoplot(future_forecast) + ggtitle("Out-of-Sample Forecast (2024–2025)")
