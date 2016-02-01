# library
library("plyr")
library("ggplot2")
library("forecast")

#-----------------------------------------------------
# 자료 불러오기
#-----------------------------------------------------
potato <- read.table("./data/potato_ttl.txt", header = T, sep = ",")[,-4]

#-----------------------------------------------------
# 연령, 지역, 성별별 평균 구매액 분석: ppt 8쪽
#-----------------------------------------------------

#-----------------------------------------------------
# 연령: ppt 8쪽
#-----------------------------------------------------

potato_age6 <- potato
potato_age6$age <- cut(x = potato$age, breaks=c(0, 20, 30, 40, 50, 60, 100))
agg_age <- merge(aggregate(formula = 보정금액 ~ 상품구분 + age, data = potato_age6 , FUN = sum), count(potato_age6, c("상품구분", "age")), by = c("상품구분", "age"))
agg_age$평균구매액 <- agg_age$보정금액 / agg_age$freq

## Plot
svg("./result/eda2_agg_age.svg", width = 10, height = 5)
ggbase <- ggplot(agg_age, aes(x = age, y = 평균구매액, group = 상품구분, color = 상품구분))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = paste0("상품", c("A", "B", "C", "D"))) + ylab("") + xlab("") + scale_x_discrete(breaks = levels(agg_age$age), labels = c("10대", "20대", "30대", "40대", "50대", "60대 이상")) + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## 유의성 test
potato_age6$age <- as.numeric(potato_age6$age)
agg_age$age <- as.numeric(agg_age$age)
sink("./result/eda2_test_age.txt")
with(agg_age, by(agg_age, 상품구분, function(x) summary(lm(평균구매액 ~ age, data = x))))
cat("\n\n\n")
with(potato_age6, by(potato_age6, 상품구분, function(x) summary(lm(보정금액 ~ age, data = x))))
sink()


#-----------------------------------------------------
# 지역: ppt 8쪽
#-----------------------------------------------------

agg_region <- merge(aggregate(formula = 보정금액 ~ 상품구분 + 구매지역, data = potato , FUN = sum), count(potato, c("상품구분", "구매지역")), by = c("상품구분", "구매지역"))
agg_region$평균구매액 <- agg_region$보정금액 / agg_region$freq

## Plot
svg("./result/eda2_agg_region.svg", width = 5, height = 5)
ggbase <- ggplot(agg_region, aes(x = 구매지역, y = 평균구매액, group = 상품구분, color = 상품구분))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## 유의성 test
potato$구매지역 <- relevel(potato$구매지역, "서울")
sink("./result/eda2_test_region.txt")
with(potato, by(potato, 상품구분, function(x) summary(lm(보정금액 ~ 구매지역, data = x))))
sink()



#-----------------------------------------------------
# 성별: ppt 8쪽
#-----------------------------------------------------

agg_gender <- merge(aggregate(formula = 보정금액 ~ 상품구분 + gender, data = potato , FUN = sum), count(potato, c("상품구분", "gender")), by = c("상품구분", "gender"))
agg_gender <- subset(agg_gender, subset = (gender != 0))
agg_gender$평균구매액 <- agg_gender$보정금액 / agg_gender$freq

## Plot
svg("./result/eda2_agg_gender.svg", width = 5, height = 5)
ggbase <- ggplot(agg_gender, aes(x = gender, y = 평균구매액, color = 상품구분, group = 상품구분))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = paste0("상품", c("A", "B", "C", "D"))) + ylab("") + xlab("") + scale_x_discrete(breaks = unique(agg_gender$gender), labels = c("남성", "여성")) + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## 유의성 test
sink("./result/eda2_test_gender.txt")
with(potato, by(potato, 상품구분, function(x) summary(lm(보정금액 ~ gender, data = x))))
sink()


#--------------------------------------------
# 주별 time-trend 그래프 ppt 9쪽
#--------------------------------------------

# 보정 전 주별trend
svg("./result/eda2_sum_weekly.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = 주, y = 주별합계/10000, color = 상품구분))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

# 보정 후 주별trend
potato_adjweek <- potato
potato_adjweek$보정주 <- potato_adjweek$보정시점 %/% 7 + 1
potato_adjweek <- merge(potato_adjweek, aggregate(보정금액 ~ 보정주 + 상품구분, data = potato_adjweek, FUN = sum), by = c("보정주", "상품구분"), all = TRUE)
colnames(potato_adjweek)[14] <- "보정금액"
colnames(potato_adjweek)[19] <- "보정주별합계"

svg("./result/eda2_sum_adjweekly.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato_adjweek, aes(x = 보정주, y = 보정주별합계/10000, color = 상품구분))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  # ggtitle("지역 - 하루 평균 구매금액") +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

#--------------------------------------------
# 일별 time-trend 그래프 ppt 10쪽
#--------------------------------------------

# 보정 전 일별trend
svg("./result/eda2_sum_daily.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = 구매시점, y = 일별합계/10000, color = 상품구분))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

# 보정 후 일별trend
svg("./result/eda2_sum_adjdaily.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = 보정시점, y = 일별합계/10000, color = 상품구분))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("상품", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

#--------------------------------------------
# 마지막 4주 변동량 ppt 10쪽
#--------------------------------------------
potato_daily <- unique(potato[, c("상품구분", "구매시점", "일별합계")])
potato_list <- list() # to save each 
for(i in 1:4){
  potato_list[[LETTERS[i]]] <- potato_daily[which(potato_daily$상품구분 == paste0("상품", LETTERS[i])),-1]
}
names(potato_list) <- LETTERS[1:4]
potato_list <- lapply(potato_list, function(x) x[order(x$구매시점),])

SNR <- function(tsdata, num = 28){
  x <- tsdata[((length(tsdata) - num + 1):length(tsdata))]
  return(data.frame(mean = mean(x), sd = sd(x), snr = mean(x)/sd(x)))
}

## result
lapply(potato_list, function(x) SNR(x$일별합계))


#--------------------------------------------
# 기온그래프 ppt 11쪽
#--------------------------------------------

# 기온vs판매액
potato_seoul <- subset(potato, 구매지역 == "서울")
potato_seoul$일별합계 <- c() # remove
potato_seoul$주별합계 <- c() # remove
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("상품구분", "보정시점"), summarize, 일별합계 = sum(보정금액)), by = c("상품구분", "보정시점"))
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("상품구분", "주"), summarize, 주별합계 = mean(일별합계), 주별평균기온 = mean(평균기온)), by = c("상품구분", "주"))
potato_seoul <- unique(potato_seoul[,c("주별합계", "상품구분", "주", "주별평균기온")])


## Plot
svg("./result/eda2_temperature_seoul.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato_seoul, aes(x = 주별평균기온, y = 주별합계/10000, group = 상품구분, color = 상품구분))
ggbase +
  geom_point() + 
  facet_wrap( ~ 상품구분, ncol = 2, scales = "free") + 
  xlab("") + ylab("") +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) + guides(color = FALSE)
dev.off()

## correlation, 기온의 유의성 test
sink("./result/eda2_test_temperature.txt")
  with(potato_seoul, by(potato_seoul, 상품구분, function(x) cor(x$주별합계, x$주별평균기온)))
  cat("\n\n")
  with(potato_seoul, by(potato_seoul, 상품구분, function(x) summary(lm(주별합계 ~ 주별평균기온, data = x))))
sink()


# 보정시점vs기온(서울)
potato_seoul <- subset(potato, ((구매지역 == "서울") & (상품구분 %in% c("상품A", "상품B"))))
potato_seoul$일별합계 <- c() # remove
potato_seoul$주별합계 <- c() # remove
potato_seoul$보정주 <- potato_seoul$보정시점 %/% 7 + 1
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("상품구분", "보정주"), summarize, 주별평균기온 = mean(평균기온)), by = c("상품구분", "보정주"))
potato_seoul <- unique(potato_seoul[,c("보정주", "주별평균기온")])

svg("./result/eda2_week_temperature.svg", width = 10, height = 4)
ggbase <- ggplot(potato_seoul, aes(x = 보정주, y = 주별평균기온))
ggbase + geom_line() + xlab("") + ylab("") + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) + guides(color = FALSE)
dev.off()




