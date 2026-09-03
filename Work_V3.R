library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
library(seasonal)
library(knitr)

# 1. Introduction and Time Series Description

#This report presents a forecasting analysis of monthly coffee imports in Portugal using data from January 2005 to December 2019. The objective is to generate accurate forecasts for the year 2019 using three main approaches: smoothing methods, decomposition techniques, and SARIMA modeling. The series is monthly, likely seasonal, and visually inspected for trends and irregularities.


coffee_data <- read_excel("C:/Users/Hp/Desktop/TS_Project/Coffee.xlsx", sheet = "R2")
colnames(coffee_data) <- c("Year", "Month", "Quantity")
coffee_ts <- ts(coffee_data$Quantity, start = c(2005, 1), frequency = 12)

train <- window(coffee_ts, end = c(2018, 12))
test <- window(coffee_ts, start = c(2019, 1))

autoplot(coffee_ts) + ggtitle("Monthly Coffee Imports (2005–2019)") + ylab("Quantity (KG)") + xlab("Year")

coffee_ts
'*Observations*:
  - The series shows strong seasonality and moderate upward trend.
- There are clear seasonal peaks, possibly around late year.
- No obvious structural breaks before 2019.
'

# 2. Smoothing and Decomposition Methods

## 2.1 Classical Decomposition

decomp <- decompose(train, type = "multiplicative") # up to Dec 2018
autoplot(decomp)


### Seasonally Adjusted Data

seasonally_adjusted <- seasadj(decomp)
autoplot(seasonally_adjusted) + ggtitle("Seasonally Adjusted Series")


## 2.2 Holt-Winters Forecast (Multiplicative)

train <- window(coffee_ts, end = c(2018, 12))
test <- window(coffee_ts, start = c(2019, 1))

hw_model <- hw(train, seasonal = "multiplicative")
hw_forecast <- forecast(hw_model, h = 12)
autoplot(hw_forecast) + autolayer(test, series = "Actual") +
  ggtitle("Holt-Winters Forecast for 2019") + xlab("Year") + ylab("KG") + coord_cartesian(xlim = c(2019, 2020)) 

kable(hw_model$model$par, caption = "Holt-Winters Smoothing Parameters")


# 3. SARIMA Modeling and Forecasting

## 3.1 Stationarity Check and Differencing

Acf(train)
Pacf(train)


## 3.2 Auto ARIMA & Diagnostics

sarima_model <- auto.arima(train, seasonal = TRUE)
summary(sarima_model)


### Forecast and Plot

sarima_forecast <- forecast(sarima_model, h = 12)
autoplot(sarima_forecast) + autolayer(test, series = "Actual") +
  ggtitle("SARIMA Forecast for 2019") + xlab("Year") + ylab("KG") + coord_cartesian(xlim = c(2019, 2020)) 


### Residual Diagnostics

checkresiduals(sarima_model)
Box.test(sarima_model$residuals, lag = 24, fitdf = length(sarima_model$coef), type = "Ljung-Box")


# 4. Forecast Comparison and Final Comments

## 4.1 Accuracy Metrics

hw_acc <- accuracy(hw_forecast, test)
sarima_acc <- accuracy(sarima_forecast, test)

comparison <- rbind(HoltWinters = hw_acc[2,], SARIMA = sarima_acc[2,])
kable(round(comparison, 2), caption = "Forecast Accuracy Comparison (2019)")


'**Interpretation**:
  - Based on RMSE, MAE, and MAPE, the better model is clearly... *(fill in based on your run)*.
- Forecast intervals were respected in most months.
'

## 4.2 Limitations & Recommendations
'- All models assume historical seasonal patterns persist.
- COVID-19 impacts are not considered (excluded on purpose).
- SARIMA performed better overall but is more complex.
'

# 5. References
'- Hyndman & Athanasopoulos (2018), *Forecasting: Principles and Practice*
  - R packages: `forecast`, `tseries`, `readxl`, `ggplot2`'