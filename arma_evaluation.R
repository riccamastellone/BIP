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

mape1 <- function(gt, preds) {
  return(max(abs(gt-round(preds))))
}

learn <- function(prod) {
  mapes <- vector(mode="numeric", length=0)
  mapes1 <- vector(mode="numeric", length=0)
  rmses <- vector(mode="numeric", length=0)
  forecasts <- vector(mode="numeric", length=0)
  
  library(forecast, quietly = T)
  
  #h <- c("Sottoarea", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
  
  if(prod == 1) {
    #write(h, file="~/bip/dataset/forecasts1.csv", ncolumns=11, sep=',', append=TRUE)
    for(s in 1:145) {
      if(s != 51 && s != 64 && s != 20) { #il 64 da un errore nella prediction, mentre il 20 ha praticamente tutti zeri nelle vendite e da mape=inf
        data <- read.csv(paste0("~/bip/dataset/prodotto_sottoarea/prodotto", prod, "_sottoarea", s, ".csv"))
        
        #outliers management
        if(s != 18 && s != 31 && s != 32 && s != 35 && s != 56 && s != 65 && s != 102 && s != 130 && s != 140 && s != 144) {
          library(tsoutliers, quietly = T)
          datats <- ts(data$vendite,start=c(2014,01),frequency=365)
          #plot.ts(datats)
          c  <- tso(datats, types = c("AO", "LS","SLS"))
          #plot(c)
          infy_ret <- c$yadj
          infy_ret <- ifelse(infy_ret < 0, 0, infy_ret)
        } else {
          infy_ret <- data$vendite
        }
        
        #infy_ret <- data$vendite
        
        lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
        #plot(as.Date(data$data, "%Y-%m-%d"), infy_ret, xlab = "Dates", ylab = "Sales", type = "l", col = "red", main=paste0("Product ", prod, " Sottoarea ",s))
        Sys.setlocale("LC_TIME", lct)
        infy_ret_train <- infy_ret[1:(length(infy_ret) - 10)]  # Train dataset
        infy_ret_test <- infy_ret[(length(infy_ret) - 10 + 1):length(infy_ret)]  # Test dataset
        fit <- auto.arima(infy_ret_train)
        arma.preds <- predict(fit, n.ahead=length(infy_ret_test))$pred
        arma.forecast <- forecast(fit, h=length(infy_ret_test))
        #plot(arma.forecast, main=paste0("Product ", prod, " Sottoarea ", s))
        stats <- accuracy(arma.preds, infy_ret_test)
        rmses <- c(rmses, stats[2])
        mapes <- c(mapes, mape(infy_ret_test, arma.preds))
        mapes1 <- c(mapes1, mape1(infy_ret_test, arma.preds))
        #f <- c(s, arma.forecast$mean[1], arma.forecast$mean[2], arma.forecast$mean[3], arma.forecast$mean[4], arma.forecast$mean[5], arma.forecast$mean[6], arma.forecast$mean[7], arma.forecast$mean[8], arma.forecast$mean[9], arma.forecast$mean[10])
        #write(f, file="~/bip/dataset/forecasts1.csv", ncolumns=11, sep=',', append=TRUE)
      }
    }
  } else {
    #write(h, file="~/bip/dataset/forecasts2.csv", ncolumns=11, sep=',', append=TRUE)
    for(s in 1:145) {
      if(s != 51 && s != 52 && s != 20 && s != 32 && s != 56 && s != 72 && s != 78 && s != 119) { #il 52 da un errore di prediction, tutti gli altri danno mape inf
        data <- read.csv(paste0("~/bip/dataset/prodotto_sottoarea/prodotto", prod, "_sottoarea", s, ".csv"))
        
        print(s)
        
        #outliers management
        #if(s != 3 && s != 6 && s != 7 && s != 15 && s != 17 && s != 71 && s != 77 && s != 83 && s != 85 && s != 115 && s != 117) {
         # library(tsoutliers, quietly = T)
          #datats <- ts(data$vendite,start=c(2014,01),frequency=365)
          ##plot.ts(datats)
          #c  <- tso(datats, types = c("AO", "LS","SLS"))
          ##plot(c)
          #infy_ret <- c$yadj
          #infy_ret <- ifelse(infy_ret < 0, 0, infy_ret)
        #} else {
        #  infy_ret <- data$vendite
        #}
        
        infy_ret <- data$vendite
        
        lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
        #plot(as.Date(data$data, "%Y-%m-%d"), infy_ret, xlab = "Dates", ylab = "Sales", type = "l", col = "red", main=paste0("Product ", prod, " Sottoarea ",s))
        Sys.setlocale("LC_TIME", lct)
        infy_ret_train <- infy_ret[1:(length(infy_ret) - 10)]  # Train dataset
        infy_ret_test <- infy_ret[(length(infy_ret) - 10 + 1):length(infy_ret)]  # Test dataset
        fit <- auto.arima(infy_ret_train)
        arma.preds <- predict(fit, n.ahead=length(infy_ret_test))$pred
        arma.forecast <- forecast(fit, h=length(infy_ret_test))
        #plot(arma.forecast, main=paste0("Product ", prod, " Sottoarea ", s))
        stats <- accuracy(arma.preds, infy_ret_test)
        rmses <- c(rmses, stats[2])
        mapes <- c(mapes, mape(infy_ret_test, arma.preds))
        mapes1 <- c(mapes1, mape1(infy_ret_test, arma.preds))
        #f <- c(s, arma.forecast$mean[1], arma.forecast$mean[2], arma.forecast$mean[3], arma.forecast$mean[4], arma.forecast$mean[5], arma.forecast$mean[6], arma.forecast$mean[7], arma.forecast$mean[8], arma.forecast$mean[9], arma.forecast$mean[10])
        #write(f, file="~/bip/dataset/forecasts2.csv", ncolumns=11, sep=',', append=TRUE)
      }
    }
  }
  return(mapes)
}  
#avg = sum(mapes)/length(mapes)

