library(ggplot2)
library(data.table)
library(forecast)
library(Rmisc)

#---------------------------------------
# ������ �ҷ�����
#---------------------------------------

df1 <- read.table("./data/Demo.txt", sep = ",", header = T)
df2 <- read.table("./data/list.txt", sep = ",", header = T)
temp_seoul <- read.table("./data/seoul.csv", sep = ",", header = T)
temp_seoul[is.na(temp_seoul)] <- 0

dt <- data.table(merge(df1, df2))
potato <- dt[ī�װ��� == "���ڽ���", ] #���ڽ���, �ǰ�����, ����ǰ �� ���ڽ������� �����ߴ�. (����:���ڴ� ������)

#---------------------------------------
# ���� ���� �Լ�
#---------------------------------------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
  
n = 4
cols = gg_color_hue(4)

#---------------------------------------
# ��ҿ��� Ȯ��: ppt 5
#---------------------------------------

potato[��ҿ���==1]
cancel.num =  potato[,.I[��ҿ���==1]] 
tb = table( potato[cancel.num,ID] ) 
# hist( tb, breaks=0:21 ,main="Histogram of ��ҿ���") #ppt���� ������� �ʾҽ��ϴ�.

100*(length( tb[tb==1] ) + length( tb[tb==2] ))/length(tb) # 84.2% �� 1�� �Ǵ� 2�� ����ߴ�
tb[tb>10] #��Ұ� 10���� �Ѵ� ����� ��ȸ
potato[ID==81048646][order(���Ž���)] # ���̵� ���� ���ű�� ��ȸ
potato[ID==26632344][order(���Ž���)] # ���̵� ���� ���ű�� ��ȸ

potato_DT <- unique(potato) # �ߺ�����
length( unique(potato_DT$ID) ) # 37862 ���� ��������
dim(potato_DT) #74171 ���� ��������


#---------------------------------------------
# �����ݾ� covariates ����� �� ppt 5�ʿ� Ȱ���� ��� 
#----------------------------------------------

potato_DT[, �����ݾ� := ���űݾ�]
potato_DT[��ҿ���==1, �����ݾ� := as.integer(���űݾ�*(-1))]
potato_DT[��ҿ���==1]

potato[ID==81048646][order(���Ž���)] #���� ��
potato_DT[ID==81048646][order(���Ž���)] #���� ��

#------------------------------------------
# ��¥����: ppt 4��
#------------------------------------------

p1 <- ggplot(potato_DT[��ǰ���� == "��ǰA"], aes(���Ž���, ��ձ��)) + geom_point(size=4, color=cols[1], shape=1) + scale_size(range = c(1,1)) + ggtitle("��ǰA") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5))) 
p2 <- ggplot(potato_DT[��ǰ���� == "��ǰB"], aes(���Ž���, ��ձ��)) + geom_point(size=4, color=cols[2], shape=2) + scale_size(range = c(1,1)) + ggtitle("��ǰB") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))
p3 <- ggplot(potato_DT[��ǰ���� == "��ǰC"], aes(���Ž���, ��ձ��)) + geom_point(size=4, color=cols[3], shape=4) + scale_size(range = c(1,1)) + ggtitle("��ǰC") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))
p4 <- ggplot(potato_DT[��ǰ���� == "��ǰD"], aes(���Ž���, ��ձ��)) + geom_point(size=4, color=cols[4], shape=20) + scale_size(range = c(1,1)) + ggtitle("��ǰD") + theme_grey()+
	theme(plot.title = element_text(size=rel(1.8)), axis.title = element_text(size=rel(1.5)))

png("./result/eda_time.png", width=1000, height=1000)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

t.a=t.b=t.c=t.d=c()
for( i in 1:182){
	t.a[i] = potato_DT[��������=="����"&��ǰ����=="��ǰA"&���Ž��� == i, ��ձ��][1]
	t.b[i] = potato_DT[��������=="����"&��ǰ����=="��ǰB"&���Ž��� == i, ��ձ��][1]
	t.c[i] = potato_DT[��������=="����"&��ǰ����=="��ǰC"&���Ž��� == i, ��ձ��][1]
	t.d[i] = potato_DT[��������=="����"&��ǰ����=="��ǰD"&���Ž��� == i, ��ձ��][1]
}

preiod=1:182
correlation.a=correlation.b=correlation.c=correlation.d=c()
M = dim(temp_seoul)[1]-182
for( j in 1:M){
	index=preiod+j
	correlation.a[j] = cor(temp_seoul$��ձ��[index], t.a )
	correlation.b[j] = cor(temp_seoul$��ձ��[index], t.b )
	correlation.c[j] = cor(temp_seoul$��ձ��[index], t.c )
	correlation.d[j] = cor(temp_seoul$��ձ��[index], t.d )
}
dates.a <- as.Date(which(max(correlation.a) == correlation.a), origin=as.Date("2014-01-01"))
dates.b <- as.Date(which(max(correlation.b) == correlation.b), origin=as.Date("2014-01-01"))
dates.c <- as.Date(which(max(correlation.c) == correlation.c), origin=as.Date("2014-01-01"))
dates.d <- as.Date(which(max(correlation.d) == correlation.d), origin=as.Date("2014-01-01"))
dates = c(dates.a, dates.b, dates.c, dates.d)

potato_DT[, �������� := ���Ž���]
potato_DT[��ǰ����=="��ǰB", �������� := ���Ž��� + as.integer( dates.b - dates.a )]
potato_DT[��ǰ����=="��ǰC", �������� := ���Ž��� + as.integer( dates.c - dates.a )]
potato_DT[��ǰ����=="��ǰD", �������� := ���Ž��� + as.integer( dates.d - dates.a )]

p <- ggplot(potato_DT, aes(��������, ��ձ��,
 colour = factor(��ǰ����), shape = factor(��ǰ����), size = factor(��ǰ����))) +
 geom_point() +
 scale_size_manual(values=c(4,4,4,4)) +
 scale_colour_manual(values=cols) +
 scale_shape_manual(values=c(1,2,4,20)) +
 theme_grey() 
p <- p + theme(legend.position='none')

ggsave(file = './result/eda_adjusted_time.png', width=10, height=10, dpi=200, plot = p)

#--------------------------------------------------
# �߰����� ( ��, �ֺ��հ�, �Ϻ��հ� ) covariates �����
#--------------------------------------------------

potato_DT[ , ��:=((���Ž��� - 1) %/% 7 ) + 1 ]
potato_DT[, �Ϻ��հ� := sum(�����ݾ�), by = c("��ǰ����",  "��������")]
potato_DT[, �ֺ��հ� := sum(�����ݾ�), by = c("��ǰ����",  "��")]

potato_week_DT <- unique( potato_DT[ ,.(��ǰ����,��,�ֺ��հ�)] )[order(��ǰ����,��)]
potato_day_DT <- unique( potato_DT[ ,.(��ǰ����,��������,�Ϻ��հ�)] )[order(��ǰ����,��������)]

potato_day_DT[��ǰ����=='��ǰC'] #check

#--------------------------------------
# ������ �ڷ� �����ϱ�
#--------------------------------------

write.csv(file = "./data/potato_ttl.txt", potato_DT, row.names=FALSE)
write.csv(file = "./data/potato_week.txt", potato_week_DT, row.names=FALSE)
write.csv(file = "./data/potato_day.txt", potato_day_DT, row.names=FALSE)

getwd()
