# library
library("plyr")
library("ggplot2")
library("forecast")

#-----------------------------------------------------
# 자료 불러오기
#-----------------------------------------------------
potato <- read.table("./data/potato_ttl.txt", header = T, sep = ",")[,-4]

#-----------------------------------------------------
# Forecast 분석 
#-----------------------------------------------------

#-----------------------------------------------------
# outsample의 모형 적합 함수
#-----------------------------------------------------

daily_outsample <- function(tsdata, tsdata_freq = 7, logscale = "none", num_forecast = 28, num_first = 0, method = "arima", method_stl = "arima", s_window = 7){
  # Input : 
  #   tsdata       : the data set used for Arima fitting
  #   tsdata_freq  : tsdata frequency to be passed to 'ts' function.
  #                  The default is 7(weekly).
  #   logscale     : the scale of log transformation you want to apply to   
  #                  'tsdata'. The default is no transformation.
  #   num_forecast : the number of periods for forecasting passed to the    
  #                  option 'h' in the 'forecast' function.
  #   num_fist     : the number of first part of data that is removed for model 
  #                  fitting
  #   method       : one of 'arima','ets', or 'stl'.
  #   method_stl   : A method within 'stl' function. Either 'arima' or 'stl'.
  #
  # Output : A list that contains
  #         1. best    : the best model
  #         2. forcast : the forecast model
  #         3. tsdata  : same as input 'tsdata'
  
  
  # all data but the last 28 days(4 weeks)
  tsdata22 <- tsdata[-c(seq_len(num_first), ((length(tsdata) - num_forecast + 1):length(tsdata)))]
  
  # data transformation
  if(is.numeric(logscale)){
    tsdata22 <- log(tsdata22, base = logscale)
  }else if(logscale == "none"){
    # nothing changes
  }else{
    stop("logscale error")
  }
  
  if(method == "arima"){
    fit_obj <- auto.arima(tsdata22, approximation = F, stepwise = F)
    fc <- forecast(fit_obj, h = num_forecast)
  }else if(method == "stl"){
    fit_obj <- stl(ts(tsdata22, freq = tsdata_freq), s.window = s_window)
    fc <- forecast.stl(fit_obj, method = method_stl, h = num_forecast)
  }else if(method == "ets"){
    fit_obj <- ets(tsdata22)
    fc <- forecast(fit_obj, h = num_forecast)
  }else{
    stop("method error")
  }
  
  if(is.numeric(logscale)){
    fc$x      <- ts(logscale^fc$x, start = num_first + 1)
    fc$fitted <- ts(logscale^fc$fitted, start = num_first + 1)
    fc$lower  <- ts(logscale^fc$lower, start = num_first + length(tsdata22) + 1) 
    fc$upper  <- ts(logscale^fc$upper, start = num_first + length(tsdata22) + 1) 
    fc$mean   <- ts(logscale^fc$mean,  start = num_first + length(tsdata22) + 1)
  } else{
    fc$x      <- ts(fc$x, start = num_first + 1) 
    fc$fitted <- ts(fc$fitted, start = num_first + 1) 
    fc$lower  <- ts(fc$lower, start = num_first + length(tsdata22) + 1) 
    fc$upper  <- ts(fc$upper, start = num_first + length(tsdata22) + 1) 
    fc$mean   <- ts(fc$mean,  start = num_first + length(tsdata22) + 1) 
  }
  result_list <- list(best22 = fit_obj, forecast22 = fc, tsdata = tsdata)
  return(result_list)
}

#-----------------------------------------------------
# control group 함수
#-----------------------------------------------------

control <- function(tsdata, num_forecast = 28, logscale = "none"){
  tsdata22 <- tsdata[-((length(tsdata) - num_forecast + 1):length(tsdata))]
  # data transformation
  if(is.numeric(logscale)){
    tsdata22 <- log(tsdata22, base = logscale)
  }else if(logscale == "none"){
    # nothing changes
  }else{
    stop("logscale error")
  }
  
  fc <- list()
  fc$mean <- rep(mean(tsdata22[(length(tsdata22) - num_forecast + 1):length(tsdata22)]), num_forecast)
  
  if(is.numeric(logscale)){
    fc$mean <- ts(logscale^fc$mean, start = length(tsdata22) + 1)
  } else{
    fc$mean <- ts(fc$mean, start = length(tsdata22) + 1) 
  }
  fc$fitted <- ts(fc$mean, start = length(tsdata22) - num_forecast + 1)
  
  return(list(forecast22 = fc, tsdata = tsdata))
}


#-----------------------------------------------------
# 그 외 함수들..
#-----------------------------------------------------

# period 개수만큼 벡터를 잘라서 합을 구해주는 함수
eqspacedsum <- function(x, period = 7){
  len <- length(x)
  out <- c()
  for(i in 1:(len/period)){
    out[i] <- sum(x[(period * (i - 1)+1):(period * i)])
  }
  return(out)
}

## error data frame을 생성하는 함수
errdfmaker <- function(dailyforecast_obj, num_forecast = 28){
  len <- length(dailyforecast_obj$tsdata)
  err22 <- abs(eqspacedsum(dailyforecast_obj$forecast22$mean - dailyforecast_obj$tsdata[(len - num_forecast + 1):len]))
  return(data.frame(errsum = err22))
}

## fitting 결과에 대한 그림 생성 함수
prediction_plot <- function(dailyforecast_obj, legend_loc = "topright", data_type, prediction = F){
  
  fc <- dailyforecast_obj$forecast22
  tsdata <- as.vector(dailyforecast_obj$tsdata)
  
  # color
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  cols = gg_color_hue(4)
  names(cols) <- LETTERS[1:4]
  
  # data
  plot(tsdata/10000, xlim = c(0, length(tsdata) + 28 * prediction), col = "gray", xlab = "", ylab = "", lwd = 2, type = "l", cex = 1, cex.axis = 2)
  
  # fitted
  lines(fc$fitted/10000, col = "black", lwd = 2)
  
  # forcasted
  lines(fc$mean/10000, col = cols[[data_type]], lwd = 2)
  
  # 95% confidence interval
  lines(fc$upper[,2]/10000, col = "blue", lwd = 2)
  lines(fc$lower[,2]/10000, col = "blue", lwd = 2)
  
  legend(legend_loc, legend = c("Data", "Fitted", "95% C.I.", "Forecast"), lty = 1, lwd = 3, col = c("gray", "black", "blue", cols[[data_type]]), cex = 2)
}


#-----------------------------------------------------
# Outsample fitting
#-----------------------------------------------------

potato_daily <- unique(potato[, c("상품구분", "구매시점", "일별합계")])
potato_list <- list() # to save each 
for(i in 1:4){
  potato_list[[LETTERS[i]]] <- potato_daily[which(potato_daily$상품구분 == paste0("상품", LETTERS[i])),-1]
}
names(potato_list) <- LETTERS[1:4]
potato_list <- lapply(potato_list, function(x) x[order(x$구매시점),])

## Fitting w/ first 4 weeks

### no transformation, control group
control_group <- list()
for(i in 1:4){
  control_group[[LETTERS[i]]] <- control(potato_list[[i]]$일별합계)
}

### no transformation, ETS
nonlog_ets <- list()
for(i in 1:4){
  nonlog_ets[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "ets")
}

### log10 transformation, ETS
log10_ets <- list()
for(i in 1:4){
  log10_ets[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "ets", logscale = 10)
}

### no transformation, ARIMA
nonlog_arima <- list()
for(i in 1:4){
  nonlog_arima[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계)
}

### log10 transformation, ARIMA
log10_arima <- list()
for(i in 1:4){
  log10_arima[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, method = "arima")
}

### no transformation, STL, ARIMA
nonlog_stl_arima <- list()
for(i in 1:4){
  nonlog_stl_arima[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "stl")
}

### log10 transformation, STL, ARIMA
log10_stl_arima <- list()
for(i in 1:4){
  log10_stl_arima[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, method = "stl")
}

### no transformation, STL, ETS
nonlog_stl_ets <- list()
for(i in 1:4){
  nonlog_stl_ets[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "stl", method_stl = "ets")
}

### log10 transformation, STL, ETS
log10_stl_ets <- list()
for(i in 1:4){
  log10_stl_ets[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, method = "stl", method_stl = "ets")
}

## Fitting w/o first 4 weeks

### no transformation, ETS
nonlog_ets_wo4 <- list()
for(i in 1:4){
  nonlog_ets_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "ets", num_first = 28)
}

### log10 transformation, ETS
log10_ets_wo4 <- list()
for(i in 1:4){
  log10_ets_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "ets", logscale = 10, num_first = 28)
}

### no transformation, ARIMA
nonlog_arima_wo4 <- list()
for(i in 1:4){
  nonlog_arima_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, num_first = 28)
}

### log10 transformation, ARIMA
log10_arima_wo4 <- list()
for(i in 1:4){
  log10_arima_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, num_first = 28)
}

### no transformation, STL, ARIMA
nonlog_stl_arima_wo4 <- list()
for(i in 1:4){
  nonlog_stl_arima_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "stl", num_first = 28)
}

### log10 transformation, STL, ARIMA
log10_stl_arima_wo4 <- list()
for(i in 1:4){
  log10_stl_arima_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, method = "stl", num_first = 28)
}

### no transformation, STL, ETS
nonlog_stl_ets_wo4 <- list()
for(i in 1:4){
  nonlog_stl_ets_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, method = "stl", method_stl = "ets", num_first = 28)
}

### log10 transformation, STL, ETS
log10_stl_ets_wo4 <- list()
for(i in 1:4){
  log10_stl_ets_wo4[[LETTERS[i]]] <- daily_outsample(potato_list[[i]]$일별합계, logscale = 10, method = "stl", method_stl = "ets", num_first = 28)
}


#-----------------------------------------------------
# Aggregate all times series models
#-----------------------------------------------------

# Make an error table
CG_err <- Map(colSums, lapply(control_group, errdfmaker))
NLE_err <- Map(colSums, lapply(nonlog_ets, errdfmaker))
L10E_err <- Map(colSums, lapply(log10_ets, errdfmaker))
NLA_err <- Map(colSums, lapply(nonlog_arima, errdfmaker))
L10A_err <- Map(colSums, lapply(log10_arima, errdfmaker))
NLSA_err <- Map(colSums, lapply(nonlog_stl_arima, errdfmaker))
L10SA_err <- Map(colSums, lapply(log10_stl_arima, errdfmaker))
NLSE_err <- Map(colSums, lapply(nonlog_stl_ets, errdfmaker))
L10SE_err <- Map(colSums, lapply(log10_stl_ets, errdfmaker))
NLEWO4_err <- Map(colSums, lapply(nonlog_ets_wo4, errdfmaker))
L10EWO4_err <- Map(colSums, lapply(log10_ets_wo4, errdfmaker))
NLAWO4_err <- Map(colSums, lapply(nonlog_arima_wo4, errdfmaker))
L10AWO4_err <- Map(colSums, lapply(log10_arima_wo4, errdfmaker))
NLSAWO4_err <- Map(colSums, lapply(nonlog_stl_arima_wo4, errdfmaker))
L10SAWO4_err <- Map(colSums, lapply(log10_stl_arima_wo4, errdfmaker))
NLSEWO4_err <- Map(colSums, lapply(nonlog_stl_ets_wo4, errdfmaker))
L10SEWO4_err <- Map(colSums, lapply(log10_stl_ets_wo4, errdfmaker))

ERR_DF <- Map(cbind, CG_err, NLE_err, L10E_err, NLA_err, L10A_err, NLSA_err, L10SA_err, NLSE_err, L10SE_err,  NLEWO4_err, L10EWO4_err, NLAWO4_err, L10AWO4_err, NLSAWO4_err, L10SAWO4_err, NLSEWO4_err, L10SEWO4_err)
ERR_DF <- lapply(ERR_DF, function(x) {
  colnames(x) <- paste0(rownames(ERR_DF[[1]]), c("_CG", "_NLE", "_L10E", "_NLA" , "_L10A", "_NLSA", "_L10SA", "_NLSE", "_L10SE", "_NLEWO4", "_L10EWO4", "_NLAWO4" , "_L10AWO4", "_NLSAWO4", "_L10SAWO4", "_NLSEWO4", "_L10SEWO4"))
  return(x)
})
ERR_DF <- rbind(ERR_DF[[1]], ERR_DF[[2]], ERR_DF[[3]], ERR_DF[[4]])
rownames(ERR_DF) <- LETTERS[1:4]
ERR_DF
# top model
#  A : log10_arima
#  B : control_group
#  C : nonlog_ets_wo4
#  D : nonlog_ets_wo4

## outsample fitting plot
svg(paste0("./result/day_top_outsample.svg"), height = 12, width = 12)
par(mfrow = c(2,2))
prediction_plot(log10_arima$A, "topleft", "A")
prediction_plot(control_group$B, "topright", "B")
prediction_plot(nonlog_ets_wo4$C, "topleft", "C")
prediction_plot(nonlog_ets_wo4$D, "topright", "D")
dev.off()

#-----------------------------------------------------
# Forecast optimized time series model
#-----------------------------------------------------

top_forecast <- list()
# A
top_forecast$A <- forecast(arima(log10(potato_list[["A"]]$일별합계), c(3,1,2)), h = 28)
top_forecast$A$forecast22 <- list()
top_forecast$A$forecast22$mean <- ts(10^top_forecast$A$mean, start = 183)
top_forecast$A$forecast22$upper <- ts(10^top_forecast$A$upper, start = 183)
top_forecast$A$forecast22$lower <- ts(10^top_forecast$A$lower, start = 183)
top_forecast$A$forecast22$fitted <- ts(10^top_forecast$A$fitted, start = 1)
top_forecast$A$tsdata <- ts(10^top_forecast$A$x, start = 1)

# B
top_forecast$B <- list()
top_forecast$B$forecast22$mean <- ts(rep(mean(potato_list[["B"]]$일별합계[155:182]), 28), start = 183)
top_forecast$B$forecast22$fitted <- ts(rep(mean(potato_list[["B"]]$일별합계[155:182]), 28), start = 156)
top_forecast$B$tsdata <- ts(potato_list[["B"]]$일별합계, start = 1)

# C
top_forecast$C <- forecast(ets(potato_list[["C"]]$일별합계[-(1:28)], "MAN", T), h = 28)
top_forecast$C$forecast22 <- list()
top_forecast$C$forecast22$mean <- ts(top_forecast$C$mean, start = 183)
top_forecast$C$forecast22$upper <- ts(top_forecast$C$upper, start = 183)
top_forecast$C$forecast22$lower <- ts(top_forecast$C$lower, start = 183)
top_forecast$C$forecast22$fitted <- ts(top_forecast$C$fitted, start = 29)
top_forecast$C$tsdata <- ts(potato_list[["C"]]$일별합계, start = 1)

# D
top_forecast$D <- forecast(ets(potato_list[["D"]]$일별합계[-(1:28)], "AAN", T), h = 28)
top_forecast$D$forecast22 <- list()
top_forecast$D$forecast22$mean <- ts(top_forecast$D$mean, start = 183)
top_forecast$D$forecast22$upper <- ts(top_forecast$D$upper, start = 183)
top_forecast$D$forecast22$lower <- ts(top_forecast$D$lower, start = 183)
top_forecast$D$forecast22$fitted <- ts(top_forecast$D$fitted, start = 29)
top_forecast$D$tsdata <- ts(potato_list[["D"]]$일별합계, start = 1)


#-----------------------------------------------------
# Plotting optimized time series model
#-----------------------------------------------------

svg(paste0("./result/day_top_forecast.svg"), height = 12, width = 12)
par(mfrow = c(2,2))
prediction_plot(top_forecast$A, "topleft", "A", prediction = T)
prediction_plot(top_forecast$B, "topright", "B", prediction = T)
prediction_plot(top_forecast$C, "topleft", "C", prediction = T)
prediction_plot(top_forecast$D, "topright", "D", prediction = T)
dev.off()


## forecast the next 4 weeks with top1 models
write.csv(round(data.frame(A = eqspacedsum(top_forecast$A$forecast22$mean),
                           B = eqspacedsum(top_forecast$B$forecast22$mean),
                           C = eqspacedsum(top_forecast$C$forecast22$mean),
                           D = eqspacedsum(top_forecast$D$forecast22$mean))/10000, 2),
          "./result/day_forecast_table.csv")



