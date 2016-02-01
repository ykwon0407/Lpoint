library(data.table)
library(forecast)
library(ggplot2)
library(Rmisc)

potato_DT <- data.table( read.csv("./data/potato_ttl.txt", sep=",", header = T) )
potato_week_DT <- data.table( read.csv("./data/potato_week.txt", sep=",", header = T) )
potato_day_DT <- data.table( read.csv("./data/potato_day.txt", sep=",", header = T) )

#----------------------------------------------
# ���� �Լ�
#----------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
  
n = 4
cols = gg_color_hue(4)

#----------------------------------------------
# Plotting day vs �Ϻ��հ�, ppt���� ����
#----------------------------------------------

# ���������� �����Ͽ� �Ϻ��հ��� �׸��� �׸���.

ggplot(potato_DT, 
aes(��������, �Ϻ��հ�, colour = factor(��ǰ����), shape = factor(��ǰ����), lty = factor(��ǰ����))) +
 geom_point() +
 geom_line() +
 scale_colour_manual(values = cols) +
 scale_shape_manual(values = c(1, 2, 3, 4)) +
 theme_grey()

# ��ǰ���� �Ϻ��հ踦 ���Ž����� ���Ͽ� �׸��� �׸���.

p1 <- ggplot(potato_DT[��ǰ���� == "��ǰA"], aes(���Ž���, (�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰA") + theme_grey()
p2 <- ggplot(potato_DT[��ǰ���� == "��ǰB"], aes(���Ž���, (�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰB") + theme_grey()
p3 <- ggplot(potato_DT[��ǰ���� == "��ǰC"], aes(���Ž���, (�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰC") + theme_grey()
p4 <- ggplot(potato_DT[��ǰ���� == "��ǰD"], aes(���Ž���, (�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰD") + theme_grey()
multiplot(p1, p2, p3, p4, cols=2)

# ��ǰ���� �Ϻ��հ踦 �α׷� ��ȯ ��, ���Ž����� ���Ͽ� �׸��� �׸���.

dev.new()
p1 <- ggplot(potato_DT[��ǰ���� == "��ǰA"], aes(���Ž���, log10(�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰA") + theme_grey()
p2 <- ggplot(potato_DT[��ǰ���� == "��ǰB"], aes(���Ž���, log10(�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰB") + theme_grey()
p3 <- ggplot(potato_DT[��ǰ���� == "��ǰC"], aes(���Ž���, log10(�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰC") + theme_grey()
p4 <- ggplot(potato_DT[��ǰ���� == "��ǰD"], aes(���Ž���, log10(�Ϻ��հ�))) + geom_point() + geom_line() + ggtitle("��ǰD") + theme_grey()
multiplot(p1, p2, p3, p4, cols=2)


#-----------------------------------------------------
# ���Ͽ� ���� �Ϻ��հ��� �߼��� ã�� �� ������? 
# Sphaghetti plot�� anova ��� : ppt 12��
#-----------------------------------------------------

p <- ggplot(potato_DT, aes(���ſ���, �Ϻ��հ�/10000, group = ��, alpha = ��, color=factor(��ǰ����))) +
 geom_line(size=1.5) + facet_grid(��ǰ����~.,scales='free') + theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank()) +
scale_x_discrete(1:7,limits=c("��","ȭ","��","��","��","��","��")) 
ggsave(file="./result/eda3_sphagetti.png", plot = p, width=7, height=7)

# ���Ͽ� ���� �Ϻ��հ谡 ���̰� ������?

sink('./result/eda3_day.txt')
cat('\n��ǰ A\n')
summary(aov(�Ϻ��հ�~���ſ���, data = potato_DT[��ǰ���� == '��ǰA']))
cat('\n��ǰ B\n')
summary(aov(�Ϻ��հ�~���ſ���, data = potato_DT[��ǰ���� == '��ǰB']))
cat('\n��ǰ C\n')
summary(aov(�Ϻ��հ�~���ſ���, data = potato_DT[��ǰ���� == '��ǰC']))
cat('\n��ǰ D\n')
summary(aov(�Ϻ��հ�~���ſ���, data = potato_DT[��ǰ���� == '��ǰD']))
sink()

# ����, �ָ� ���ο� ���� �Ϻ��հ谡 ���̰� ������?

potato_DT[,�ָ�����:=0]
potato_DT[(���ſ��� == 6 | ���ſ��� == 7), �ָ�����:=1]
potato_DT

sink('./result/eda3_weekend.txt')
cat('\n��ǰ A\n')
summary(aov(�Ϻ��հ�~�ָ�����, data = potato_DT[��ǰ���� == '��ǰA']))
cat('\n��ǰ B\n')
summary(aov(�Ϻ��հ�~�ָ�����, data = potato_DT[��ǰ���� == '��ǰB']))
cat('\n��ǰ C\n')
summary(aov(�Ϻ��հ�~�ָ�����, data = potato_DT[��ǰ���� == '��ǰC']))
cat('\n��ǰ D\n')
summary(aov(�Ϻ��հ�~�ָ�����, data = potato_DT[��ǰ���� == '��ǰD']))
sink()




