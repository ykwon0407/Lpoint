library(data.table)
library(forecast)
library(ggplot2)

potato_DT <- data.table( read.csv("./data/potato_ttl.txt", sep=",", header = T) )
potato_week_DT <- data.table( read.csv("./data/potato_week.txt", sep=",", header = T) )
potato_day_DT <- data.table( read.csv("./data/potato_day.txt", sep=",", header = T) )
NAME <- data.table( '상품'=c("상품A","상품B","상품C","상품D") )

#-----------------------------------------------------------------
# 색깔함수
#-----------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
  
n = 4
cols = gg_color_hue(4)

#-----------------------------------------------------------------
# 단위를 주로 하여, plot을 그린다.
# week_all은 주별합계를 나타낸 그래프.
# week_goods은 상품에 따라서 주별합계를 나타낸 그래프.
# week_goods_log는 상품에 따라서 로그변환한 주별합계를 나타낸 그래프.
#-----------------------------------------------------------------

plot_week_all <- ggplot(potato_DT, aes(주, 주별합계, colour = factor(상품구분), shape = factor(상품구분), lty = factor(상품구분))) +
 geom_point() + 
 geom_line() + 
 scale_colour_manual(values = cols) +
 scale_shape_manual(values = c(1, 2, 3, 4)) +
 theme_grey()

plot_week_goods <- ggplot(potato_DT, aes(주, 주별합계/10000)) + geom_point() + geom_line()  + theme_grey() + facet_wrap(~상품구분 , scales='free', ncol=2)

plot_week_goods_log <- ggplot(potato_DT, aes(주, log10(주별합계))) + geom_point() + geom_line() + theme_grey() + facet_wrap(~상품구분 , scales='free', ncol=2)

#--------------------------------------------------------------------------------------------------------
# 시계열 분석을 위한 함수 time_series는 길이가 26인 벡터를 input을 받아서 ETS 혹은 arima를 계산 해준다.
# out_sample을 통해서 validation error를 구할 수 있다.
# snr() 함수는 signal to noise ratio를 계산해주는 함수이다. x는 길이 4인 벡터를 input으로 사용한다.
#--------------------------------------------------------------------------------------------------------

time_series <- function(vec, log_trans=FALSE, out_sample=TRUE, isETS=TRUE, promotion_effect = FALSE ){
	if(out_sample){
		x <- vec[1:22]
	}
	if(promotion_effect){
		print('yes')
		x <- x[5:length(x)]
	}
	
	if(log_trans){
		x <- log10(x)
	}

	if(isETS){
		fit <- ets(y = x)
	}else{
		fit <- auto.arima(x = x)
	}
	fit.forecast <- forecast(fit, h=4)
	fitted_value <- fitted(fit)
	forecast_value <- summary(fit.forecast)$'Point Forecast'

	return(list(fitted = fitted_value, fore = forecast_value))
}

time_series_err <- function(vec, log_trans=FALSE, out_sample=TRUE, isETS=TRUE, promotion_effect = FALSE ){
	res_values <- lapply(vecs, time_series, log_trans=log_trans, out_sample=out_sample, isETS=isETS, promotion_effect = promotion_effect)
	res = rep(0,4)
	for( i in 1:4){
		if(log_trans){
			res[i] <- sum(abs(10^res_values[[i]]$fore-tail(vecs[[i]],4) ))
		}else{
			res[i] <- sum(abs(res_values[[i]]$fore-tail(vecs[[i]],4) ))
		}
	}
	return(res)
}

err_mean_model_function <- function(vecs, log_trans = FALSE){
	tail_list = lapply(vecs, tail, n=4)
	res = rep(0,4)
	if(!log_trans){
		mean_list = lapply(lapply(lapply(vecs, tail, n=8), head, 4), mean)
		for(i in 1:4){
			res[i] <- sum(abs(mean_list[[i]]-tail_list[[i]]))
		}
	}else{
		mean_list = lapply(lapply(lapply(lapply(vecs, tail, n=8), head, 4), log10), mean)
		for(i in 1:4){
			res[i] <- sum(abs(exp(mean_list[[i]])-tail_list[[i]]))
		}
	}
	return(res)
}

snr <- function(x, multiLog=1){
	x = switch(multiLog, x, log(x), log(log(x)))
	x = tail(x,4)
	return( round(c(mean(x), sd(x), mean(x)/sd(x)),2) )
}

snr_table <- function(vecs){
	L0 <- rbindlist(data.table(lapply(vecs, snr)))
	L1 <- rbindlist(data.table(lapply(vecs, snr, multiLog=2)))
	L2 <- rbindlist(data.table(lapply(vecs, snr, multiLog=3)))
	T <- cbind(NAME,t(L0),t(L1),t(L2))
	return(T)
}

argmin <- function(x){
	seq = 1:length(x)
	return( seq[(min(x)==x)][1] )
}

#----------------------------------------------------------
# 분석
#----------------------------------------------------------

vecs = lapply(split.data.frame(potato_week_DT,potato_week_DT$상품구분), "[[", "주별합계")

# SNR table을 다음과 같이 만든다.
snr_table_all <- snr_table(vecs)
colnames(snr_table_all) <- c('상품','normal_mean','normal_sd','normal_snr','log_mean','log_sd','log_snr','loglog_mean','loglog_sd','loglog_snr')

# fitting 결과는 다음과 같이 불러온다.
res_normal_out_ETS <- lapply(vecs, time_series, log_trans=FALSE, out_sample=TRUE, isETS=TRUE)
res_log_out_ETS <- lapply(vecs, time_series, log_trans=TRUE, out_sample=TRUE, isETS=TRUE)
res_normal_out_arima <- lapply(vecs, time_series, log_trans=FALSE, out_sample=TRUE, isETS=FALSE)
res_log_out_arima <- lapply(vecs, time_series, log_trans=TRUE, out_sample=TRUE, isETS=FALSE)

# out_sample error는 다음으로 불러온다.
err_normal_out_ETS <- time_series_err(vecs, log_trans=FALSE, out_sample=TRUE, isETS=TRUE)
err_log_out_ETS <- time_series_err(vecs, log_trans=TRUE, out_sample=TRUE, isETS=TRUE)
err_normal_out_arima <- time_series_err(vecs, log_trans=FALSE, out_sample=TRUE, isETS=FALSE)
err_log_out_arima <- time_series_err(vecs, log_trans=TRUE, out_sample=TRUE, isETS=FALSE)
err_mean_model <- err_mean_model_function(vecs)
err_log_mean_model <- err_mean_model_function(vecs, log_trans = TRUE)

# result의 취합은 다음과 같이한다.
result_num <- cbind(err_normal_out_ETS, err_log_out_ETS, err_normal_out_arima, err_log_out_arima,err_mean_model)/10000
out_sample <- cbind(NAME, result_num)

# best model은 다음과 같이 계산한다.
min_error <- data.table(err=apply(result_num, 1, min))
min_model <- apply(result_num, 1, argmin)
model_name <- c('ets', 'ets with log', 'arima', 'arima with log', 'mean') 
best_model <- cbind(NAME, model = model_name[min_model], min_error)

#----------------------------------------------------------
# 추가 분석: 처음 promotion effect 4주를 제거해서 분석하는 경우
#---------------------------------------------------------

# out_sample error는 다음으로 불러온다.
err_normal_out_pro_ETS <- time_series_err(vecs, log_trans=FALSE, out_sample=TRUE, isETS=TRUE, promotion_effect = TRUE)
err_log_out_pro_ETS <- time_series_err(vecs, log_trans=TRUE, out_sample=TRUE, isETS=TRUE, promotion_effect = TRUE)
err_normal_out_pro_arima <- time_series_err(vecs, log_trans=FALSE, out_sample=TRUE, isETS=FALSE, promotion_effect = TRUE)
err_log_out_pro_arima <- time_series_err(vecs, log_trans=TRUE, out_sample=TRUE, isETS=FALSE, promotion_effect = TRUE)

# result의 취합은 다음과 같이한다.
result_pro_num <- cbind(err_normal_out_pro_ETS, err_log_out_pro_ETS, err_normal_out_pro_arima, err_log_out_pro_arima,err_mean_model)/10000
out_pro_sample <- cbind(NAME, result_pro_num)

# best model은 다음과 같이 계산한다.
min_pro_error <- data.table(err=apply(result_pro_num, 1, min))
min_pro_model <- apply(result_pro_num, 1, argmin)
best_pro_model <- cbind(NAME, model = model_name[min_pro_model], min_pro_error)

# Table about snr, out_sample error, and best_model are saved in "snr.csv", 'out_sample.csv', and "best_model.csv".
# _pro_ means that model do not consider promotion effect, so remove first four data.

snr_table_all
write.csv(file = "./result/week_snr.csv", snr_table_all, row.names=FALSE)
out_sample
write.csv(file = "./result/week_out_sample.csv", out_sample, row.names=FALSE)
best_model
write.csv(file = "./result/week_best_model.csv", best_model, row.names=FALSE)
out_pro_sample
write.csv(file = "./result/week_out_pro_sample.csv", out_pro_sample, row.names=FALSE)
best_pro_model
write.csv(file = "./result/week_best_pro_model.csv", best_pro_model, row.names=FALSE)



