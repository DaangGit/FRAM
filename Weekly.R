#### Libraries ####

library(readxl)
library(quantmod)
library(xts)
library(tseries)
library(rugarch)
library(rmgarch)

#### CAPM Model ####

# Data Collection
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
NSE <- na.omit(NSE)

titan <- getSymbols.yahoo("TITAN.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
titan<- na.omit(titan)

torntpower <- getSymbols.yahoo("TORNTPOWER.NS", from ="2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
torntpower <- na.omit(torntpower)

# T-Bill Data
D_W_M_T_Bills <- read_excel("W-T-Bills.xlsx")
D_W_M_T_Bills <- as.data.frame(D_W_M_T_Bills)
D_W_M_T_Billsxts <- xts(D_W_M_T_Bills[,-1], order.by = as.Date(D_W_M_T_Bills$Date))

# Excess Returns
exNSE <- as.data.frame(weeklyReturn(NSE$NSEI.Close)[-1, ] - D_W_M_T_Bills$`T-Bill% (Weekly)`)
extitan <- as.data.frame(weeklyReturn(titan$TITAN.NS.Close)[-1, ] - D_W_M_T_Bills$`T-Bill% (Weekly)`)
extorntpower <- as.data.frame(weeklyReturn(torntpower$TORNTPOWER.NS.Close)[-1, ] - D_W_M_T_Bills$`T-Bill% (Weekly)`)

# Excess Returns
exret1 <- cbind(exNSE, extitan)
exret2 <- cbind(exNSE, extorntpower)
colnames(exret1) <- c('NSEI.ExcessReturns','TITAN.ExcessReturns')
colnames(exret2) <- c('NSEI.ExcessReturns','TORNTPOWER.ExcessReturns')


# Return Calculation
returns1 <- as.xts(exret1)
# head(returns1)
returns2 <- as.xts(exret2)
# head(returns2)

## CAPM Model ##
regression1 <- lm(TITAN.ExcessReturns ~ NSEI.ExcessReturns, data.frame(returns1[]))
summary(regression1)

regression2 <- lm(TORNTPOWER.ExcessReturns ~ NSEI.ExcessReturns, data.frame(returns2[]))
summary(regression2)

#### END: CAPM Model ####





#### ARIMA Model ####

# Return Calculation
returns_titan <- as.xts(tail(data.frame(titan$TITAN.NS.Close),-1)/head(data.frame(titan$TITAN.NS.Close),-1)-1, frequency = 52)
returns_titan <- na.omit(returns_titan)
returns_torntpower <- as.xts(tail(data.frame(torntpower$TORNTPOWER.NS.Close),-1)/head(data.frame( torntpower$TORNTPOWER.NS.Close),-1)-1, frequency = 52)
returns_torntpower <- na.omit(returns_torntpower)

# Data Manipulation
colnames(returns_titan) <- "returns_titan"
colnames(returns_torntpower) <- "returns_torntpower"
# mean(returns_titan)
# var(returns_titan)
# mean(returns_torntpower)
# var(returns_torntpower)

# Data Visualization
plot(titan$TITAN.NS.Close)
plot(returns_titan)
plot(torntpower$TORNTPOWER.NS.Close)
plot(returns_torntpower)

# Stationarity Test
adf.test(returns_titan, alternative = "stationary")
adf.test(returns_torntpower, alternative = "stationary")

# AR & MA Order Identification
# Titan
plot(acf(returns_titan , lag.max = 10))
plot(pacf(returns_titan , lag.max = 10))
arima_final1 <- arima(returns_titan, order= c(1,0,1))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

# Torrent Power
plot(acf(returns_torntpower , lag.max = 100))
plot(pacf(returns_torntpower , lag.max = 100))
arima_final2 <- arima(returns_torntpower, order= c(1,0,1))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

#### END: ARIMA Model ####





#### GARCH & EGARCH Models ####

# Data Collection
titan1 <- getSymbols("TITAN.NS", from = "2020-04-01", to = "2022-03-31", periodicity = "weekly")
head(TITAN.NS)

torntpower1 <- getSymbols("TORNTPOWER.NS", from ="2020-04-01", to = "2022-03-31", periodicity = "weekly") 
head(TORNTPOWER.NS)

# Return Calculation
rtitan <- dailyReturn(TITAN.NS)
rtorntpower <- dailyReturn(TORNTPOWER.NS)

# Implementing Univariate GARCH
ug_spec = ugarchspec()
ug_spec

# Implementing EGARCH
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

# Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = rtitan) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = rtorntpower) 
ugfit2

egfit1 = ugarchfit(spec = eg_spec, data = rtitan)
egfit1

egfit2 = ugarchfit(spec = eg_spec, data = rtorntpower)
egfit2

# Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10)
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

egforecast1 = ugarchforecast(egfit1, n.ahead=10) 
egforecast1

egforecast2 = ugarchforecast(egfit2, n.ahead=10) 
egforecast2
