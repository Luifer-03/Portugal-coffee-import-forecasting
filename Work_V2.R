library(readxl)
library(forecast)
library(ggplot2)

# Load dataset (assumes data starts at row 6)
coffee_data <- read_excel("C:/Users/Hp/Desktop/TS_Project/Coffee.xlsx", sheet = "R2")

# Rename and convert to time series
colnames(coffee_data) <- c("Year", "Month", "Quantity")
coffee_data$Date <- as.Date(paste(coffee_data$Year, coffee_data$Month, "1", sep = "-"), "%Y-%b-%d")
coffee_ts <- ts(coffee_data$Quantity, start = c(2005, 1), frequency = 12)

# Train/test split
train <- window(coffee_ts, end = c(2018, 12))
test <- window(coffee_ts, start = c(2019, 1))

################################################################################

# Decomposition
decomp <- decompose(train, type = "multiplicative")
autoplot(decomp)

# Holt-Winters (multiplicative seasonal)
hw_model <- hw(train, seasonal = "multiplicative")
hw_forecast <- forecast(hw_model, h = 12)

# Plot
autoplot(hw_forecast) + autolayer(test, series = "Actual") +
  ggtitle("Holt-Winters Forecast for 2019") + xlab("Year") + ylab("KG") + coord_cartesian(xlim = c(2019, 2020)) 

# Accuracy
accuracy(hw_forecast, test)

################################################################################

# Fit SARIMA on training data
sarima_model <- auto.arima(train, seasonal = TRUE)
summary(sarima_model)

# Forecast 2019
sarima_forecast <- forecast(sarima_model, h = 12)

# Plot
autoplot(sarima_forecast) + autolayer(test, series = "Actual") +
  ggtitle("SARIMA Forecast for 2019") + xlab("Year") + ylab("KG") + coord_cartesian(xlim = c(2019, 2020))

# Accuracy
accuracy(sarima_forecast, test)

# Residual diagnostics
checkresiduals(sarima_model)
Box.test(sarima_model$residuals, lag = 24, fitdf = length(sarima_model$coef), type = "Ljung-Box")

################################################################################

# Combine and compare accuracy metrics
hw_acc <- accuracy(hw_forecast, test)
sarima_acc <- accuracy(sarima_forecast, test)

comparison <- rbind(HoltWinters = hw_acc[2, ], SARIMA = sarima_acc[2, ])
round(comparison, 2)

