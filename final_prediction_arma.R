#install.packages("tseries")
#install.packages("forecast")
#install.packages("fpp")
#install.packages("tsoutliers")

mape <- function(gt, preds) {
  one_over_n <- 1/length(preds)
  at_mean <- sum(gt)/length(gt)
  i <- 1
  sommatoria <- 0
  for(pred in preds) {
    sommatoria <- sommatoria + (abs(gt[i] - round(pred)) / at_mean)
  }
  return(one_over_n * sommatoria)
}

learn <- function(prod) {
  mapes <- vector(mode="numeric", length=0)
  rmses <- vector(mode="numeric", length=0)
  forecasts <- vector(mode="numeric", length=0)
  
  library(forecast, quietly = T)
  
  h <- c("Sottoarea", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
  
  if(prod == 1) {
    write(h, file="~/bip/dataset/future_forecasts1.csv", ncolumns=11, sep=',', append=TRUE)
    for(s in 1:145) {
      #if(s != 51 && s != 64 && s != 20) { #il 64 da un errore nella prediction, mentre il 20 ha praticamente tutti zeri nelle vendite e da mape=inf
      if(s != 51 && s != 64) { #il 64 da un errore nella prediction
        if(s != 20) {
          data <- read.csv(paste0("~/bip/dataset/prodotto_sottoarea/prodotto", prod, "_sottoarea", s, ".csv"))
          
          #if(s != 18 && s != 31 && s != 32 && s != 35 && s != 56 && s != 65 && s != 102 && s != 130 && s != 140 && s != 144) {
          #  library(tsoutliers, quietly = T)
          #  datats <- ts(data$vendite,start=c(2014,01),frequency=365)
          #  #plot.ts(datats)
          #  c  <- tso(datats, types = c("AO", "LS","SLS"))
          #  #plot(c)
          #  infy_ret <- c$yadj
          #  infy_ret <- ifelse(infy_ret < 0, 0, infy_ret)
          #} else {
          #  infy_ret <- data$vendite
          #}
          
          infy_ret <- data$vendite
          
          #data$day <- as.factor(weekdays(as.Date(data$data, "%Y-%m-%d")))
          #days <- data$day[1:nrow(data)]
          #model = model.matrix(~as.factor(days) + 0)
          #xreg1_train <- model[1:(nrow(model) -10), ]  # Train dataset
          #xreg1_test <- model[(nrow(model) - 10 + 1):nrow(model), ]  # Test dataset
          
          lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
          #plot(as.Date(data$data, "%Y-%m-%d"), infy_ret, xlab = "Dates", ylab = "Sales", type = "l", col = "red", main=paste0("Product ", prod, " Sottoarea ",s))
          Sys.setlocale("LC_TIME", lct)
          #infy_ret_train <- infy_ret[1:(length(infy_ret) - 10)]  # Train dataset
          #infy_ret_test <- infy_ret[(length(infy_ret) - 10 + 1):length(infy_ret)]  # Test dataset
          fit <- auto.arima(infy_ret)
          #arma.preds <- predict(fit)
          arma.forecast <- forecast(fit, h=10)
          plot(arma.forecast, main=paste0("Product ", prod, " Sottoarea ", s))
          #stats <- accuracy(armax.preds$pred, infy_ret_test)
          #rmses <- c(rmses, stats[2])
          #mapes <- c(mapes, mape(infy_ret_test, armax.preds$pred))
          #print(s)
          f <- c(s, round(arma.forecast$mean[1]), round(arma.forecast$mean[2]), round(arma.forecast$mean[3]), round(arma.forecast$mean[4]), round(arma.forecast$mean[5]), round(arma.forecast$mean[6]), round(arma.forecast$mean[7]), round(arma.forecast$mean[8]), round(arma.forecast$mean[9]), round(arma.forecast$mean[10]))
        } else {
          f <- c(s, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }
        write(f, file="~/bip/dataset/future_forecasts1.csv", ncolumns=11, sep=',', append=TRUE)
      }
    }
  } else {
    write(h, file="~/bip/dataset/future_forecasts2.csv", ncolumns=11, sep=',', append=TRUE)
    for(s in 1:145) {
      if(s != 51 && s != 52 && s != 20 && s != 32 && s != 56 && s != 72 && s != 78 && s != 119) { #il 52 da un errore di prediction, tutti gli altri danno mape inf
        data <- read.csv(paste0("~/bip/dataset/prodotto_sottoarea/prodotto", prod, "_sottoarea", s, ".csv"))
        
        infy_ret <- data$vendite
        #infy_ret_train <- infy_ret[1:(length(infy_ret) - 10)]  # Train dataset
        #infy_ret_test <- infy_ret[(length(infy_ret) - 10 + 1):length(infy_ret)]  # Test dataset
        fit <- auto.arima(infy_ret)
        #arma.preds <- predict(fit, n.ahead=10)$pred
        arma.forecast <- forecast(fit, h=10)
        plot(arma.forecast, main=paste0("Product ", prod, " Sottoarea ", s))
        #stats <- accuracy(arma.preds, infy_ret_test)
        #rmses <- c(rmses, stats[2])
        #mapes <- c(mapes, mape(infy_ret_test, arma.preds))
        f <- c(s, round(arma.forecast$mean[1]), round(arma.forecast$mean[2]), round(arma.forecast$mean[3]), round(arma.forecast$mean[4]), round(arma.forecast$mean[5]), round(arma.forecast$mean[6]), round(arma.forecast$mean[7]), round(arma.forecast$mean[8]), round(arma.forecast$mean[9]), round(arma.forecast$mean[10]))
        write(f, file="~/bip/dataset/future_forecasts2.csv", ncolumns=11, sep=',', append=TRUE)
      }
    }
  }
  return(mapes)
}  
#avg = sum(mapes)/length(mapes)

