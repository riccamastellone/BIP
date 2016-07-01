#idea presa da http://www.r-bloggers.com/forecasting-stock-returns-using-arima-model-with-exogenous-variable-in-r/

#preprocessing:
#nella data 30/06/2014 c'è un outlier da eliminare...
#procedura adottata: settato al valore massimo per quel prodotto per quella sottoarea

install.packages("tseries")
install.packages("forecast")
install.packages("fpp")
install.packages("tsoutliers")

mape <- function(gt, preds) {
  one_over_n <- 1/length(preds)
  at_mean <- sum(gt)/length(gt)
  i <- 1
  sommatoria <- 0
  for(pred in preds) {
    sommatoria <- sommatoria + (abs(gt[i] - pred) / at_mean)
  }
  return(one_over_n * sommatoria)
}

#questa parte è per l'analisi solo delle vendite
data <- read.csv("~/bip/dataset/prodotto_sottoarea/prodotto2_sottoarea2.csv")
summary(data)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
plot(as.Date(data$data, "%Y-%m-%d"), data$vendite, xlab = "Dates", ylab = "Sales", type = "l", col = "red", main = "Product 1 Area 68")
Sys.setlocale("LC_TIME", lct)

#stationarity test
library(fpp, quietly = T)
library(tseries, quietly = T)
Acf(data$vendite, main="ACF (Autocorrelation Function) Test")
Pacf(data$vendite, main="PACF (Partial Autocorrelation Function) Test")
adf.test(data$vendite)
kpss.test(data$vendite, null="Level")

#infy_ret <- 100 * diff(log(data$vendite))
infy_ret <- data$vendite

library(forecast, quietly = T)
infy_ret_train <- infy_ret[1:(0.9 * length(infy_ret))]  # Train dataset
infy_ret_test <- infy_ret[(0.9 * length(infy_ret) + 1):length(infy_ret)]  # Test dataset
fit <- auto.arima(infy_ret_train)
fit <- arima(infy_ret_train, order = c(2, 0, 2))
fit <- arima(infy_ret_train, order = c(5, 1, 3))
arma.preds <- predict(fit, n.ahead = (length(infy_ret) - (0.9 * length(infy_ret))))$pred
arma.forecast <- forecast(fit, h = 20)
arma.forecast
plot(arma.forecast, main = "Product 1 Area 68")
stats <- accuracy(arma.preds, infy_ret_test)
stats
mape(infy_ret_test, arma.preds)



#questa parte è un tentativo di passare ad un ARMAX, cioè inserendo
#anche exhogenous variables per avere più info ed ottenere un modello migliore
#in questo caso ho provato solo il giorno della settimana
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data$day <- as.factor(weekdays(as.Date(data$data, "%Y-%m-%d")))
days <- data$day[1:nrow(data)]
#model = model.matrix(~as.factor(days) + 0) #the + 0 is to specify that I want to keep all the columns and don't want the first one to collapse in the intercept

model_giorno <- model.matrix(~as.factor(data$giorno_della_settimana) + 0)
model_mese <- model.matrix(~as.factor(data$mese) + 0)
model_festivo <- model.matrix(~data$festivo + 0)
model <- cbind(model_giorno, model_festivo)
model <- cbind(model_giorno, model_mese)
model <- cbind(model_mese, model_festivo)
model <- cbind(model_giorno, model_mese, model_festivo)

model = model.matrix(~as.factor(days) + 0) #the + 0 is to specify that I want to keep all the columns and don't want the first one to collapse in the intercept

model <- cbind(data$vendite, model_giorno)
model <- cbind(data$vendite, model_festivo)
model <- cbind(data$vendite, data$giorno_della_settimana)

require("tseries")
#kpss.test(Nile)

#res <- tso(y = Nile, tsmethod = "auto.arima")
#res
#kpss.test(res$yadj)

library(tsoutliers, quietly = T)
datats <- ts(data$vendite,start=c(2014,01),frequency=365)
kpss.test(datats)

Acf(res$yadj, main="ACF (Autocorrelation Function) Test")
Pacf(res$yadj, main="PACF (Partial Autocorrelation Function) Test")

res <- tso(datats, tsmethod = "auto.arima")
res

kpss.test(res$yadj)
kpss.test(res$yadj, null="Trend")

datats <- ts(data$vendite,start=c(2014,01),frequency=365)
plot.ts(datats)
c  <- tso(datats, types = c("AO", "LS","SLS"))
plot(c)

library(tsoutliers, quietly = T)
datats <- ts(model,start=c(2014,01),frequency=365)
plot.ts(datats)
c  <- tso(datats, types = c("AO"))
#c  <- tso(datats, types = c("AO", "LS","SLS"))
plot(c)

xreg1_test <- model[(0.9 * nrow(model) + 1):nrow(model), ]  # Test dataset
xreg1_train <- model[1:(0.9 * nrow(model)), ]  # Train dataset
colnames(xreg1_train) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre", "Festivo") 
colnames(xreg1_test) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre", "Festivo") 

#mese,fes
colnames(xreg1_train) <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre", "Festivo") 
colnames(xreg1_test) <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre", "Festivo") 


colnames(xreg1_train) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre") 
colnames(xreg1_test) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")

colnames(xreg1_train) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica") 
colnames(xreg1_test) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica") 

colnames(xreg1_train) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Festivo") 
colnames(xreg1_test) <- c("Lunedi", "Martedi", "Mercoledi", "Giovedi", "Venerdi", "Sabato", "Domenica", "Festivo") 

colnames(xreg1_test) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday") 
colnames(xreg1_train) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday") 

Sys.setlocale("LC_TIME", lct)

fit2 <- arima(infy_ret_train, order = c(0, 0, 1), xreg = xreg1_train, include.mean=FALSE) #0.3838028
fit2 <- arima(infy_ret_train, order = c(1, 0, 0), xreg = xreg1_train, include.mean=FALSE) #0.3838028
fit2 <- arima(infy_ret_train, order = c(1, 0, 1), xreg = xreg1_train, include.mean=FALSE) #0.2535211
fit2 <- arima(infy_ret_train, order = c(1, 0, 2), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(2, 0, 1), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(2, 0, 2), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(1, 0, 3), xreg = xreg1_train, include.mean=FALSE) #0.2535211
fit2 <- arima(infy_ret_train, order = c(3, 0, 1), xreg = xreg1_train, include.mean=FALSE) #0.2535211
fit2 <- arima(infy_ret_train, order = c(2, 0, 3), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(3, 0, 2), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(3, 0, 3), xreg = xreg1_train, include.mean=FALSE)
fit2 <- arima(infy_ret_train, order = c(4, 0, 1), xreg = xreg1_train, include.mean=FALSE) #0.2112676

fit2 <- arima(infy_ret_train, order = c(4, 0, 2), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(4, 0, 3), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(4, 0, 4), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(4, 0, 5), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(2, 0, 4), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(2, 0, 5), xreg = xreg1_train, include.mean=FALSE) #0.2992958
fit2 <- arima(infy_ret_train, order = c(1, 0, 4), xreg = xreg1_train, include.mean=FALSE) #0.2535211
fit2 <- arima(infy_ret_train, order = c(1, 0, 5), xreg = xreg1_train, include.mean=FALSE) #0.2535211
fit2 <- arima(infy_ret_train, order = c(3, 0, 4), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(3, 0, 5), xreg = xreg1_train, include.mean=FALSE) #0.2112676
fit2 <- arima(infy_ret_train, order = c(5, 0, 1), xreg = xreg1_train, include.mean=FALSE)
fit2 <- arima(infy_ret_train, order = c(4, 1, 5), xreg = xreg1_train, include.mean=FALSE) #0.2112676

fit2 <- auto.arima(infy_ret_train, xreg=xreg1_train, stepwise = FALSE, trace = TRUE, approximation = TRUE, max.order = 12)
armax.preds <- predict(fit2, newxreg=xreg1_test)
#armax.preds <- predict(fit2, n.ahead = (nrow(model) - (0.9 * nrow(model))), newxreg = xreg1_test)
#xreg1_forecast <- rbind(c(0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0),c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0))
armax.forecast <- forecast(fit2, xreg=xreg1_test)
#fit1.forecast <- forecast(fit2, h = 10, xreg = xreg1_forecast)

armax.preds$pred
armax.forecast
#fit1.preds$pred
infy_ret_test

#qui dovrei creare i dati dei giorni della settimana per i prossimi giorni, da usare al posto di xreg1_test
#plot(forecast(fit2, h = 10, xreg = xreg1_test), main = "ARIMAX forecasts of INFY returns")
plot(armax.forecast, main="ARMAX forecast")
#accuracy(fit1.preds$pred, infy_ret_test)[1]
stats <-- accuracy(armax.preds$pred, infy_ret_test)
stats

mape(infy_ret_test, armax.preds$pred)
                              