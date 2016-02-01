library(ggplot2)
library(data.table)
library(forecast)
library(Rmisc)

#---------------------------------------
# 데이터 불러오기
#---------------------------------------

df1 <- read.table("./data/Demo.txt", sep = ",", header = T)
df2 <- read.table("./data/list.txt", sep = ",", header = T)
temp_seoul <- read.table("./data/seoul.csv", sep = ",", header = T)
temp_seoul[is.na(temp_seoul)] <- 0

dt <- data.table(merge(df1, df2))
potato <- dt[카테고리 == "감자스낵", ] #감자스낵, 건강음료, 유제품 중 감자스낵으로 선택했다. (이유:감자는 맛있음)

#---------------------------------------
# 색깔 조정 함수
#---------------------------------------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
  
n = 4
cols = gg_color_hue(4)

#---------------------------------------
# 취소여부 확인: ppt 5
#---------------------------------------

potato[취소여부==1]
cancel.num =  potato[,.I[취소여부==1]] 
tb = table( potato[cancel.num,ID] ) 
# hist( tb, breaks=0:21 ,main="Histogram of 취소여부") #ppt에는 사용하지 않았습니다.

100*(length( tb[tb==1] ) + length( tb[tb==2] ))/length(tb) # 84.2% 가 1번 또는 2번 취소했다
tb[tb>10] #취소가 10번이 넘는 사람을 조회
potato[ID==81048646][order(구매시점)] # 아이디 별로 구매기록 조회
potato[ID==26632344][order(구매시점)] # 아이디 별로 구매기록 조회

potato_DT <- unique(potato) # 중복제거
length( unique(potato_DT$ID) ) # 37862 명의 구매정보
dim(potato_DT) #74171 건의 구매정보


#---------------------------------------------
# 보정금액 covariates 만들기 및 ppt 5쪽에 활용한 결과 
#----------------------------------------------

potato_DT[, 보정금액 := 구매금액]
potato_DT[취소여부==1, 보정금액 := as.integer(구매금액*(-1))]
potato_DT[취소여부==1]

potato[ID==81048646][order(구매시점)] #보정 전
potato_DT[ID==81048646][order(구매시점)] #보정 후

#------------------------------------------
# 날짜보정: ppt 4쪽
#------------------------------------------

p1 <- ggplot(potato_DT[상품구분 == "상품A"], aes(구매시점, 평균기온)) + geom_point(size=4, color=cols[1], shape=1) + scale_size(range = c(1,1)) + ggtitle("상품A") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5))) 
p2 <- ggplot(potato_DT[상품구분 == "상품B"], aes(구매시점, 평균기온)) + geom_point(size=4, color=cols[2], shape=2) + scale_size(range = c(1,1)) + ggtitle("상품B") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))
p3 <- ggplot(potato_DT[상품구분 == "상품C"], aes(구매시점, 평균기온)) + geom_point(size=4, color=cols[3], shape=4) + scale_size(range = c(1,1)) + ggtitle("상품C") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))
p4 <- ggplot(potato_DT[상품구분 == "상품D"], aes(구매시점, 평균기온)) + geom_point(size=4, color=cols[4], shape=20) + scale_size(range = c(1,1)) + ggtitle("상품D") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))

png("./result/eda_time.png", width=1000, height=1000)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

t.a=t.b=t.c=t.d=c()
for( i in 1:182){
	t.a[i] = potato_DT[구매지역=="서울"&상품구분=="상품A"&구매시점 == i, 평균기온][1]
	t.b[i] = potato_DT[구매지역=="서울"&상품구분=="상품B"&구매시점 == i, 평균기온][1]
	t.c[i] = potato_DT[구매지역=="서울"&상품구분=="상품C"&구매시점 == i, 평균기온][1]
	t.d[i] = potato_DT[구매지역=="서울"&상품구분=="상품D"&구매시점 == i, 평균기온][1]
}

preiod=1:182
correlation.a=correlation.b=correlation.c=correlation.d=c()
M = dim(temp_seoul)[1]-182
for( j in 1:M){
	index=preiod+j
	correlation.a[j] = cor(temp_seoul$평균기온[index], t.a )
	correlation.b[j] = cor(temp_seoul$평균기온[index], t.b )
	correlation.c[j] = cor(temp_seoul$평균기온[index], t.c )
	correlation.d[j] = cor(temp_seoul$평균기온[index], t.d )
}
dates.a <- as.Date(which(max(correlation.a) == correlation.a), origin=as.Date("2014-01-01"))
dates.b <- as.Date(which(max(correlation.b) == correlation.b), origin=as.Date("2014-01-01"))
dates.c <- as.Date(which(max(correlation.c) == correlation.c), origin=as.Date("2014-01-01"))
dates.d <- as.Date(which(max(correlation.d) == correlation.d), origin=as.Date("2014-01-01"))
dates = c(dates.a, dates.b, dates.c, dates.d)

potato_DT[, 보정시점 := 구매시점]
potato_DT[상품구분=="상품B", 보정시점 := 구매시점 + as.integer( dates.b - dates.a )]
potato_DT[상품구분=="상품C", 보정시점 := 구매시점 + as.integer( dates.c - dates.a )]
potato_DT[상품구분=="상품D", 보정시점 := 구매시점 + as.integer( dates.d - dates.a )]

p <- ggplot(potato_DT, aes(보정시점, 평균기온,
 colour = factor(상품구분), shape = factor(상품구분), size = factor(상품구분))) +
 geom_point() +
 scale_size_manual(values=c(4,4,4,4)) +
 scale_colour_manual(values=cols) +
 scale_shape_manual(values=c(1,2,4,20)) +
 theme_grey() 
p <- p + theme(legend.position='none')

ggsave(file = './result/eda_adjusted_time.png', width=10, height=10, dpi=200, plot = p)

#--------------------------------------------------
# 추가정보 ( 주, 주별합계, 일별합계 ) covariates 만들기
#--------------------------------------------------

potato_DT[ , 주:=((구매시점 - 1) %/% 7 ) + 1 ]
potato_DT[, 일별합계 := sum(보정금액), by = c("상품구분",  "보정시점")]
potato_DT[, 주별합계 := sum(보정금액), by = c("상품구분",  "주")]

potato_week_DT <- unique( potato_DT[ ,.(상품구분,주,주별합계)] )[order(상품구분,주)]
potato_day_DT <- unique( potato_DT[ ,.(상품구분,보정시점,일별합계)] )[order(상품구분,보정시점)]

potato_day_DT[상품구분=='상품C'] #check

#--------------------------------------
# 가공한 자료 저장하기
#--------------------------------------

write.csv(file = "./data/potato_ttl.txt", potato_DT, row.names=FALSE)
write.csv(file = "./data/potato_week.txt", potato_week_DT, row.names=FALSE)
write.csv(file = "./data/potato_day.txt", potato_day_DT, row.names=FALSE)

getwd()

