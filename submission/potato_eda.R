library(ggplot2)
library(data.table)
library(plyr)

#---------------------------------------------------------------
# 상품별 색깔을 통일하는 함수. 
#---------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
n = 4
cols = gg_color_hue(4)

#---------------------------------------------------------------
# frequency, portion 추출하여 저장하는 함수
#---------------------------------------------------------------

ftn.freq <- function(xvec) {
  if (sum(is.na(xvec))> 0) {  
    ftable1 = as.data.frame(ftable(xvec, exclude=NULL)) ; 
  } else {
    ftable1 = as.data.frame(ftable(xvec)) ; 
  }
  labels = ftable1[,1] ;
  ftable2 = ftable1[,2]/length(xvec) ;
  #prettyNum(c(123,1234),big.mark=",", preserve.width="none")
  print = sprintf("%.1f%% (%s)", 100*as.numeric(ftable2),
                  prettyNum(ftable1[,2],big.mark=",", preserve.width="none")) ;
  return(data.frame(label = labels, print = print, freq = ftable1[ ,2], portion = ftable2))
}

ftn.money <- function(xvec, factorvec) {
  table.x = tapply( X=xvec, INDEX=factorvec, 
                    FUN=sum ) ;
  labels = names(table.x) ;
  portion.x = as.numeric(table.x/sum(table.x)) ;
  
  print = sprintf("%.1f%% (%s)", 100*portion.x,
                  prettyNum(round(table.x/10000),big.mark=",", preserve.width="none")) ;
  return(data.frame(label = labels, print = print, freq = table.x, portion = portion.x))
}

#---------------------------------------------------------------
# 미리 정리해둔 감자데이터를 불러온다.
#---------------------------------------------------------------
ptl = read.table("./data/potato_ttl.txt", sep=",", header=T) ;

#---------------------------------------------------------------
# 구매기록별 나이, 성별, 지역 정보 저장: ppt 6쪽
#---------------------------------------------------------------

#table(ptl$age)
ptl.age10 = cut(x=ptl$age, breaks=c(10, 20, 30, 40, 50, 60, 100) ) 

table.record.age10 = ftn.freq(ptl.age10) ;
table.record.gender = ftn.freq(ptl$gender) ;
table.record.region = ftn.freq(ptl$구매지역) ;

write.csv(table.record.age10, "./result/eda_table_record_age10.csv", row.names=FALSE) ;
write.csv(table.record.gender, "./result/eda_table_record_gender.csv", row.names=FALSE) ;
write.csv(table.record.region, "./result/eda_table_record_region.csv", row.names=FALSE) ;

svg("./result/eda_portion_record_age10.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.age10$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.record.age10), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;


#---------------------------------------------------------------
# 사람별 나이, 성별, 지역 정보 저장
# 감자 구매자 정보만 추출 : ppt 6쪽
#---------------------------------------------------------------

demo.raw = cbind(ptl[ ,c("ID", "gender", "구매지역")], age10=ptl.age10) ;
demo.raw.uniq = unique(demo.raw) ;
demo = demo.raw[ ,c("ID", "gender", "age10")] ;
demo.uniq = unique(demo) ;

ID.duplicated = demo.raw.uniq$ID[duplicated(demo.raw.uniq$ID)] ;
ind.dupl = demo.raw.uniq$ID %in% ID.duplicated ;
demo.raw.uniq$구매지역 = as.character(demo.raw.uniq$구매지역) ;
demo.raw.uniq$구매지역[ind.dupl] = "혼합" ;
demo.uniq2 = unique(demo.raw.uniq) ;

table.human.age10 = ftn.freq(demo.uniq2$age10) ;
table.human.gender = ftn.freq(demo.uniq2$gender) ;
table.human.region = ftn.freq(demo.uniq2$구매지역) ;
write.csv(table.human.age10, "./result/eda_table_human_age10.csv", row.names=FALSE) ;
write.csv(table.human.gender, "./result/eda_table_human_gender.csv", row.names=FALSE) ;
write.csv(table.human.region, "./result/eda_table_human_region.csv", row.names=FALSE) ;

svg("./result/eda_portion_human_age10.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.human.age10$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.human.age10), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;
gray.colors(n=5, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)

#---------------------------------------------------------------
## 각 individual은 구매 기록 건이 얼마나 있는가?: ppt 7쪽 전처리과정
#---------------------------------------------------------------

table.human.buy = as.data.frame(table(ptl$ID[ptl$취소여부==0])) ;
colnames(table.human.buy) = c("ID", "buy") ;
buycut = cut(x = table.human.buy$buy, breaks = c(0, 1, 5, 10, max(table.human.buy$buy)) ) ;
table.human.buy = cbind(table.human.buy, buycut) ;
demo.uniq3 = merge(demo.uniq2, table.human.buy, by="ID") ;
head(demo.uniq3)

# ptl에 buycut 삽입
ptl2 = merge(x=ptl, y=demo.uniq3[ ,c("ID", "age10", "buycut")], by="ID") ;

#---------------------------------------------------------------
# 사람들을 -1건/-5건/-10건/초과 로 나누어서 전체 사람, 전체 구매건,
# 전체 구매금액에서의 기여도 체크: ppt 7쪽 
#---------------------------------------------------------------

table.human.buycut = ftn.freq(table.human.buy$buycut) ;
write.csv(table.human.buycut, "./result/eda_table_human_buycut.csv", row.names=FALSE) ;
table.record.buycut = ftn.freq(ptl2$buycut) ;
write.csv(table.record.buycut, "./result/eda_table_record_buycut.csv", row.names=FALSE) ; # 실제 ppt에는 고려 안했음.
table.record.buymoney = ftn.money(xvec=ptl2$보정금액, factorvec=ptl2$buycut)
write.csv(table.record.buymoney, "./result/eda_table_record_buymoney.csv", row.names=FALSE) ;

svg("./result/eda_portion_human_buycut.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.human.buycut$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.human.buycut), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;

svg("./result/eda_portion_record_buymoney.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.buymoney$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.record.buymoney), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;


#---------------------------------------------------------------
# 제품 A, B, C, D별로는?: ppt 7쪽
#---------------------------------------------------------------

table.record.A.buymoney = ftn.money(xvec=ptl2$보정금액[ptl2$상품구분=="상품A"],
                                 factorvec=ptl2$buycut[ptl2$상품구분=="상품A"])
write.csv(table.record.A.buymoney, "./result/eda_table_record_A_buymoney.csv", row.names=FALSE) ;
table.record.B.buymoney = ftn.money(xvec=ptl2$보정금액[ptl2$상품구분=="상품B"],
                                    factorvec=ptl2$buycut[ptl2$상품구분=="상품B"])
write.csv(table.record.B.buymoney, "./result/eda_table_record_B_buymoney.csv", row.names=FALSE) ;
table.record.C.buymoney = ftn.money(xvec=ptl2$보정금액[ptl2$상품구분=="상품C"],
                                    factorvec=ptl2$buycut[ptl2$상품구분=="상품C"])
write.csv(table.record.C.buymoney, "./result/eda_table_record_C_buymoney.csv", row.names=FALSE) ;
table.record.D.buymoney = ftn.money(xvec=ptl2$보정금액[ptl2$상품구분=="상품D"],
                                    factorvec=ptl2$buycut[ptl2$상품구분=="상품D"])
write.csv(table.record.D.buymoney, "./result/eda_table_record_D_buymoney.csv", row.names=FALSE) ;


#---------------------------------------------------------------
# 각 상품별 점유율 plot: ppt 7쪽
#---------------------------------------------------------------

palette <- colorRampPalette(colors=c(gray.colors(1, start=0.9,end=0.9), cols[1]))
Acols <- palette(nrow(table.record.A.buymoney))

svg("./result/eda_portion_record_A_buymoney.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.A.buymoney$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=Acols)
dev.off() ;

palette <- colorRampPalette(colors=c(gray.colors(1, start=0.9,end=0.9), cols[2]))
Bcols <- palette(nrow(table.record.B.buymoney))

svg("./result/eda_portion_record_B_buymoney.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.B.buymoney$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=Bcols)
dev.off() ;

palette <- colorRampPalette(colors=c(gray.colors(1, start=0.9,end=0.9), cols[3]))
Ccols <- palette(nrow(table.record.C.buymoney))

svg("./result/eda_portion_record_C_buymoney.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.C.buymoney$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=Ccols)
dev.off() ;

palette <- colorRampPalette(colors=c(gray.colors(1, start=0.9,end=0.9), cols[4]))
Dcols <- palette(nrow(table.record.D.buymoney))

svg("./result/eda_portion_record_D_buymoney.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.D.buymoney$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=Dcols)
dev.off() ;


#---------------------------------------------------------------
# 브랜드별 충성도?
# ID는 unique하게 37862명이 있다. ID.uniq에 있음
## 본인의 구매 기록이 한 번인 사람?
# 먼저 duplicated index 뽑고 : 아래의 결과는 ppt에는 없음.
#---------------------------------------------------------------

purchase.ID.duplicated = unique(ptl$ID[duplicated(ptl$ID)]) ;
length(purchase.ID.duplicated) # 두번 이상 산 사람의 구매기록은 얼마나? 9841명
ind.ptl.ID.duplicated = ptl$ID %in% purchase.ID.duplicated ;

sum(ind.ptl.ID.duplicated)
#[1] 46150 # 46150 건은 2번 이상 산 사람들의 기록이다.
sum(!ind.ptl.ID.duplicated)
#[1] 28021 # 28021 건은 1번만 산 사람들의 기록이므로, 한번만 산 28021명은 뭘 샀는가 궁금하다.

ptl.1buy = ptl[ !ind.ptl.ID.duplicated, ] ; # 1번만 산 사람만을 모았다.
summary(ptl.1buy[ ,c("상품구분","구매건수", "보정금액")]) ;

# 상품구분        구매건수         보정금액     
# 상품A: 6879   Min.   : 1.000   Min.   :-20000  
# 상품B: 4582   1st Qu.: 1.000   1st Qu.:  2000  
# 상품C: 5135   Median : 1.000   Median :  2000  
# 상품D:11425   Mean   : 1.356   Mean   :  2499  
#               3rd Qu.: 1.000   3rd Qu.:  2000  
#               Max.   :96.000   Max.   :144500  

#plot(ptl.1buy[ , ]) #시간이 너무 오래 걸립니다.
#boxplot(ptl.1buy$구매건수)

ptl.1buy[ ptl.1buy$구매건수 > 10,  ]$상품구분
# 1번만 구매한 고객 중에서 상품을 10개 이상 산 사람들이 구매한 상품은 C또는 D이다.

summary(as.factor(ptl.1buy$구매건수))
# 1      2      3     4     5      6     7     8     9     10    11    12    13    14    15 
# 22926  3468   726   385   173    99    32    46    14    45     5    11     7     8     1 
# 16    17    20    22    23    30    32    40    96 
# 61     4     2     1     2     1     2     1     1 
# D를 한 번만 산 사람들을 보정할 필요가 있다.
# 한 번이라도 A,..를 산 사람

