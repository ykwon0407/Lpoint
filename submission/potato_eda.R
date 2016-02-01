library(ggplot2)
library(data.table)
library(plyr)

#---------------------------------------------------------------
# ��ǰ�� ������ �����ϴ� �Լ�. 
#---------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
n = 4
cols = gg_color_hue(4)

#---------------------------------------------------------------
# frequency, portion �����Ͽ� �����ϴ� �Լ�
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
# �̸� �����ص� ���ڵ����͸� �ҷ��´�.
#---------------------------------------------------------------
ptl = read.table("./data/potato_ttl.txt", sep=",", header=T) ;

#---------------------------------------------------------------
# ���ű�Ϻ� ����, ����, ���� ���� ����: ppt 6��
#---------------------------------------------------------------

#table(ptl$age)
ptl.age10 = cut(x=ptl$age, breaks=c(10, 20, 30, 40, 50, 60, 100) ) 

table.record.age10 = ftn.freq(ptl.age10) ;
table.record.gender = ftn.freq(ptl$gender) ;
table.record.region = ftn.freq(ptl$��������) ;

write.csv(table.record.age10, "./result/eda_table_record_age10.csv", row.names=FALSE) ;
write.csv(table.record.gender, "./result/eda_table_record_gender.csv", row.names=FALSE) ;
write.csv(table.record.region, "./result/eda_table_record_region.csv", row.names=FALSE) ;

svg("./result/eda_portion_record_age10.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.record.age10$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.record.age10), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;


#---------------------------------------------------------------
# ����� ����, ����, ���� ���� ����
# ���� ������ ������ ���� : ppt 6��
#---------------------------------------------------------------

demo.raw = cbind(ptl[ ,c("ID", "gender", "��������")], age10=ptl.age10) ;
demo.raw.uniq = unique(demo.raw) ;
demo = demo.raw[ ,c("ID", "gender", "age10")] ;
demo.uniq = unique(demo) ;

ID.duplicated = demo.raw.uniq$ID[duplicated(demo.raw.uniq$ID)] ;
ind.dupl = demo.raw.uniq$ID %in% ID.duplicated ;
demo.raw.uniq$�������� = as.character(demo.raw.uniq$��������) ;
demo.raw.uniq$��������[ind.dupl] = "ȥ��" ;
demo.uniq2 = unique(demo.raw.uniq) ;

table.human.age10 = ftn.freq(demo.uniq2$age10) ;
table.human.gender = ftn.freq(demo.uniq2$gender) ;
table.human.region = ftn.freq(demo.uniq2$��������) ;
write.csv(table.human.age10, "./result/eda_table_human_age10.csv", row.names=FALSE) ;
write.csv(table.human.gender, "./result/eda_table_human_gender.csv", row.names=FALSE) ;
write.csv(table.human.region, "./result/eda_table_human_region.csv", row.names=FALSE) ;

svg("./result/eda_portion_human_age10.svg", width=6, height=2.2) ;
barplot(height=as.matrix(table.human.age10$portion), beside=FALSE, horiz=TRUE, border=FALSE,
        col=gray.colors(n=nrow(table.human.age10), start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
dev.off() ;
gray.colors(n=5, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)

#---------------------------------------------------------------
## �� individual�� ���� ��� ���� �󸶳� �ִ°�?: ppt 7�� ��ó������
#---------------------------------------------------------------

table.human.buy = as.data.frame(table(ptl$ID[ptl$��ҿ���==0])) ;
colnames(table.human.buy) = c("ID", "buy") ;
buycut = cut(x = table.human.buy$buy, breaks = c(0, 1, 5, 10, max(table.human.buy$buy)) ) ;
table.human.buy = cbind(table.human.buy, buycut) ;
demo.uniq3 = merge(demo.uniq2, table.human.buy, by="ID") ;
head(demo.uniq3)

# ptl�� buycut ����
ptl2 = merge(x=ptl, y=demo.uniq3[ ,c("ID", "age10", "buycut")], by="ID") ;

#---------------------------------------------------------------
# ������� -1��/-5��/-10��/�ʰ� �� ����� ��ü ���, ��ü ���Ű�,
# ��ü ���űݾ׿����� �⿩�� üũ: ppt 7�� 
#---------------------------------------------------------------

table.human.buycut = ftn.freq(table.human.buy$buycut) ;
write.csv(table.human.buycut, "./result/eda_table_human_buycut.csv", row.names=FALSE) ;
table.record.buycut = ftn.freq(ptl2$buycut) ;
write.csv(table.record.buycut, "./result/eda_table_record_buycut.csv", row.names=FALSE) ; # ���� ppt���� ���� ������.
table.record.buymoney = ftn.money(xvec=ptl2$�����ݾ�, factorvec=ptl2$buycut)
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
# ��ǰ A, B, C, D���δ�?: ppt 7��
#---------------------------------------------------------------

table.record.A.buymoney = ftn.money(xvec=ptl2$�����ݾ�[ptl2$��ǰ����=="��ǰA"],
                                 factorvec=ptl2$buycut[ptl2$��ǰ����=="��ǰA"])
write.csv(table.record.A.buymoney, "./result/eda_table_record_A_buymoney.csv", row.names=FALSE) ;
table.record.B.buymoney = ftn.money(xvec=ptl2$�����ݾ�[ptl2$��ǰ����=="��ǰB"],
                                    factorvec=ptl2$buycut[ptl2$��ǰ����=="��ǰB"])
write.csv(table.record.B.buymoney, "./result/eda_table_record_B_buymoney.csv", row.names=FALSE) ;
table.record.C.buymoney = ftn.money(xvec=ptl2$�����ݾ�[ptl2$��ǰ����=="��ǰC"],
                                    factorvec=ptl2$buycut[ptl2$��ǰ����=="��ǰC"])
write.csv(table.record.C.buymoney, "./result/eda_table_record_C_buymoney.csv", row.names=FALSE) ;
table.record.D.buymoney = ftn.money(xvec=ptl2$�����ݾ�[ptl2$��ǰ����=="��ǰD"],
                                    factorvec=ptl2$buycut[ptl2$��ǰ����=="��ǰD"])
write.csv(table.record.D.buymoney, "./result/eda_table_record_D_buymoney.csv", row.names=FALSE) ;


#---------------------------------------------------------------
# �� ��ǰ�� ������ plot: ppt 7��
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
# �귣�庰 �漺��?
# ID�� unique�ϰ� 37862���� �ִ�. ID.uniq�� ����
## ������ ���� ����� �� ���� ���?
# ���� duplicated index �̰� : �Ʒ��� ����� ppt���� ����.
#---------------------------------------------------------------

purchase.ID.duplicated = unique(ptl$ID[duplicated(ptl$ID)]) ;
length(purchase.ID.duplicated) # �ι� �̻� �� ����� ���ű���� �󸶳�? 9841��
ind.ptl.ID.duplicated = ptl$ID %in% purchase.ID.duplicated ;

sum(ind.ptl.ID.duplicated)
#[1] 46150 # 46150 ���� 2�� �̻� �� ������� ����̴�.
sum(!ind.ptl.ID.duplicated)
#[1] 28021 # 28021 ���� 1���� �� ������� ����̹Ƿ�, �ѹ��� �� 28021���� �� ��°� �ñ��ϴ�.

ptl.1buy = ptl[ !ind.ptl.ID.duplicated, ] ; # 1���� �� ������� ��Ҵ�.
summary(ptl.1buy[ ,c("��ǰ����","���ŰǼ�", "�����ݾ�")]) ;

# ��ǰ����        ���ŰǼ�         �����ݾ�     
# ��ǰA: 6879   Min.   : 1.000   Min.   :-20000  
# ��ǰB: 4582   1st Qu.: 1.000   1st Qu.:  2000  
# ��ǰC: 5135   Median : 1.000   Median :  2000  
# ��ǰD:11425   Mean   : 1.356   Mean   :  2499  
#               3rd Qu.: 1.000   3rd Qu.:  2000  
#               Max.   :96.000   Max.   :144500  

#plot(ptl.1buy[ , ]) #�ð��� �ʹ� ���� �ɸ��ϴ�.
#boxplot(ptl.1buy$���ŰǼ�)

ptl.1buy[ ptl.1buy$���ŰǼ� > 10,  ]$��ǰ����
# 1���� ������ ���� �߿��� ��ǰ�� 10�� �̻� �� ������� ������ ��ǰ�� C�Ǵ� D�̴�.

summary(as.factor(ptl.1buy$���ŰǼ�))
# 1      2      3     4     5      6     7     8     9     10    11    12    13    14    15 
# 22926  3468   726   385   173    99    32    46    14    45     5    11     7     8     1 
# 16    17    20    22    23    30    32    40    96 
# 61     4     2     1     2     1     2     1     1 
# D�� �� ���� �� ������� ������ �ʿ䰡 �ִ�.
# �� ���̶� A,..�� �� ���
