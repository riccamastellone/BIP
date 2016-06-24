#idea presa da http://www.r-bloggers.com/forecasting-stock-returns-using-arima-model-with-exogenous-variable-in-r/


#preprocessing:
#1)sostituire """ con " nel file... si trovano solo nel campo data
#2)il risultato migliore l'ho ottenuto andando a "livellare" l'outlier con
#vendite = 28 mettendolo a 9.

install.packages("tseries")
install.packages("forecast")
install.packages("fpp")
#questa parte è per l'analisi solo delle vendite
data <- read.csv("~/Desktop/stock/dataset_polimi_andrea3_prodotto1_sottoarea68.csv")
summary(data)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(fpp, quietly = T)
Acf(data$vendite)
Pacf(data$vendite)
plot(as.Date(data$data, "%Y-%m-%d"), data$vendite, xlab = "Dates", ylab = "Sales", type = "l", col = "red", main = "Product 1 Area 68")
Sys.setlocale("LC_TIME", lct)
library(tseries, quietly = T)
adf.test(data$vendite)
#infy_ret <- 100 * diff(log(data$vendite))
infy_ret <- data$vendite
library(forecast, quietly = T)
infy_ret_train <- infy_ret[1:(0.9 * length(infy_ret))]  # Train dataset
infy_ret_test <- infy_ret[(0.9 * length(infy_ret) + 1):length(infy_ret)]  # Test dataset
fit <- arima(infy_ret_train, order = c(2, 0, 2))
arma.preds <- predict(fit, n.ahead = (length(infy_ret) - (0.9 * length(infy_ret))))$pred
arma.forecast <- forecast(fit, h = 20)
arma.forecast
plot(arma.forecast, main = "Product 1 Area 68")
accuracy(arma.preds, infy_ret_test)[2]  # RMSE values


#questa parte è un tentativo di passare ad un ARMAX, cioè inserendo
#anche exhogenous variables per avere più info ed ottenere un modello migliore
#in questo caso ho provato solo il giorno della settimana
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data$day <- as.factor(weekdays(as.Date(data$data, "%Y-%m-%d")))
days <- data$day[1:nrow(data)]
model = model.matrix(~as.factor(days) + 0) #the + 0 is to specify that I want to keep all the columns and don't want the first one to collapse in the intercept
xreg1_test <- model[(0.9 * nrow(model) + 1):nrow(model), ]  # Test dataset
xreg1_train <- model[1:(0.9 * nrow(model)), ]  # Train dataset
colnames(xreg1_test) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday") 
colnames(xreg1_train) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday") 
Sys.setlocale("LC_TIME", lct)

fit2 <- arima(infy_ret_train, order = c(2, 0, 7), xreg = xreg1_train, include.mean=FALSE)
fit1.preds <- predict(fit2, n.ahead = (nrow(model) - (0.9 * nrow(model))), newxreg = xreg1_test)
#fit1.preds <- forecast(fit2, h = 10, xreg = xreg1_train)

#qui dovrei creare i dati dei giorni della settimana per i prossimi giorni, da usare al posto di xreg1_test
plot(forecast(fit2, h = 10, xreg = xreg1_test), main = "ARIMAX forecasts of INFY returns")
accuracy(fit1.preds$pred, infy_ret_test)[2]



