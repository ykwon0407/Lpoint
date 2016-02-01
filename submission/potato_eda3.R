library(data.table)
library(forecast)
library(ggplot2)
library(Rmisc)

potato_DT <- data.table( read.csv("./data/potato_ttl.txt", sep=",", header = T) )
potato_week_DT <- data.table( read.csv("./data/potato_week.txt", sep=",", header = T) )
potato_day_DT <- data.table( read.csv("./data/potato_day.txt", sep=",", header = T) )

#----------------------------------------------
# 색깔 함수
#----------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
  
n = 4
cols = gg_color_hue(4)

#----------------------------------------------
# Plotting day vs 일별합계, ppt에는 없음
#----------------------------------------------

# 보정시점을 고려하여 일별합계의 그림을 그린다.

ggplot(potato_DT, 
aes(보정시점, 일별합계, colour = factor(상품구분), shape = factor(상품구분), lty = factor(상품구분))) +
 geom_point() +
 geom_line() +
 scale_colour_manual(values = cols) +
 scale_shape_manual(values = c(1, 2, 3, 4)) +
 theme_grey()

# 상품별로 일별합계를 구매시점에 대하여 그림을 그린다.

p1 <- ggplot(potato_DT[상품구분 == "상품A"], aes(구매시점, (일별합계))) + geom_point() + geom_line() + ggtitle("상품A") + theme_grey()
p2 <- ggplot(potato_DT[상품구분 == "상품B"], aes(구매시점, (일별합계))) + geom_point() + geom_line() + ggtitle("상품B") + theme_grey()
p3 <- ggplot(potato_DT[상품구분 == "상품C"], aes(구매시점, (일별합계))) + geom_point() + geom_line() + ggtitle("상품C") + theme_grey()
p4 <- ggplot(potato_DT[상품구분 == "상품D"], aes(구매시점, (일별합계))) + geom_point() + geom_line() + ggtitle("상품D") + theme_grey()
multiplot(p1, p2, p3, p4, cols=2)

# 상품별로 일별합계를 로그로 변환 후, 구매시점에 대하여 그림을 그린다.

dev.new()
p1 <- ggplot(potato_DT[상품구분 == "상품A"], aes(구매시점, log10(일별합계))) + geom_point() + geom_line() + ggtitle("상품A") + theme_grey()
p2 <- ggplot(potato_DT[상품구분 == "상품B"], aes(구매시점, log10(일별합계))) + geom_point() + geom_line() + ggtitle("상품B") + theme_grey()
p3 <- ggplot(potato_DT[상품구분 == "상품C"], aes(구매시점, log10(일별합계))) + geom_point() + geom_line() + ggtitle("상품C") + theme_grey()
p4 <- ggplot(potato_DT[상품구분 == "상품D"], aes(구매시점, log10(일별합계))) + geom_point() + geom_line() + ggtitle("상품D") + theme_grey()
multiplot(p1, p2, p3, p4, cols=2)


#-----------------------------------------------------
# 요일에 따라서 일별합계의 추세를 찾을 수 있을까? 
# Sphaghetti plot과 anova 결과 : ppt 12쪽
#-----------------------------------------------------

p <- ggplot(potato_DT, aes(구매요일, 일별합계/10000, group = 주, alpha = 주, color=factor(상품구분))) +
 geom_line(size=1.5) + facet_grid(상품구분~.,scales='free') + theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank()) +
scale_x_discrete(1:7,limits=c("월","화","수","목","금","토","일")) 
ggsave(file="./result/eda3_sphagetti.png", plot = p, width=7, height=7)

# 요일에 따라서 일별합계가 차이가 있을까?

sink('./result/eda3_day.txt')
cat('\n상품 A\n')
summary(aov(일별합계~구매요일, data = potato_DT[상품구분 == '상품A']))
cat('\n상품 B\n')
summary(aov(일별합계~구매요일, data = potato_DT[상품구분 == '상품B']))
cat('\n상품 C\n')
summary(aov(일별합계~구매요일, data = potato_DT[상품구분 == '상품C']))
cat('\n상품 D\n')
summary(aov(일별합계~구매요일, data = potato_DT[상품구분 == '상품D']))
sink()

# 주중, 주말 여부에 따라서 일별합계가 차이가 있을까?

potato_DT[,주말여부:=0]
potato_DT[(구매요일 == 6 | 구매요일 == 7), 주말여부:=1]
potato_DT

sink('./result/eda3_weekend.txt')
cat('\n상품 A\n')
summary(aov(일별합계~주말여부, data = potato_DT[상품구분 == '상품A']))
cat('\n상품 B\n')
summary(aov(일별합계~주말여부, data = potato_DT[상품구분 == '상품B']))
cat('\n상품 C\n')
summary(aov(일별합계~주말여부, data = potato_DT[상품구분 == '상품C']))
cat('\n상품 D\n')
summary(aov(일별합계~주말여부, data = potato_DT[상품구분 == '상품D']))
sink()





