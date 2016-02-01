# library
library("plyr")
library("ggplot2")
library("forecast")

#-----------------------------------------------------
# �ڷ� �ҷ�����
#-----------------------------------------------------
potato <- read.table("./data/potato_ttl.txt", header = T, sep = ",")[,-4]

#-----------------------------------------------------
# ����, ����, ������ ��� ���ž� �м�: ppt 8��
#-----------------------------------------------------

#-----------------------------------------------------
# ����: ppt 8��
#-----------------------------------------------------

potato_age6 <- potato
potato_age6$age <- cut(x = potato$age, breaks=c(0, 20, 30, 40, 50, 60, 100))
agg_age <- merge(aggregate(formula = �����ݾ� ~ ��ǰ���� + age, data = potato_age6 , FUN = sum), count(potato_age6, c("��ǰ����", "age")), by = c("��ǰ����", "age"))
agg_age$��ձ��ž� <- agg_age$�����ݾ� / agg_age$freq

## Plot
svg("./result/eda2_agg_age.svg", width = 10, height = 5)
ggbase <- ggplot(agg_age, aes(x = age, y = ��ձ��ž�, group = ��ǰ����, color = ��ǰ����))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = paste0("��ǰ", c("A", "B", "C", "D"))) + ylab("") + xlab("") + scale_x_discrete(breaks = levels(agg_age$age), labels = c("10��", "20��", "30��", "40��", "50��", "60�� �̻�")) + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## ���Ǽ� test
potato_age6$age <- as.numeric(potato_age6$age)
agg_age$age <- as.numeric(agg_age$age)
sink("./result/eda2_test_age.txt")
with(agg_age, by(agg_age, ��ǰ����, function(x) summary(lm(��ձ��ž� ~ age, data = x))))
cat("\n\n\n")
with(potato_age6, by(potato_age6, ��ǰ����, function(x) summary(lm(�����ݾ� ~ age, data = x))))
sink()


#-----------------------------------------------------
# ����: ppt 8��
#-----------------------------------------------------

agg_region <- merge(aggregate(formula = �����ݾ� ~ ��ǰ���� + ��������, data = potato , FUN = sum), count(potato, c("��ǰ����", "��������")), by = c("��ǰ����", "��������"))
agg_region$��ձ��ž� <- agg_region$�����ݾ� / agg_region$freq

## Plot
svg("./result/eda2_agg_region.svg", width = 5, height = 5)
ggbase <- ggplot(agg_region, aes(x = ��������, y = ��ձ��ž�, group = ��ǰ����, color = ��ǰ����))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## ���Ǽ� test
potato$�������� <- relevel(potato$��������, "����")
sink("./result/eda2_test_region.txt")
with(potato, by(potato, ��ǰ����, function(x) summary(lm(�����ݾ� ~ ��������, data = x))))
sink()



#-----------------------------------------------------
# ����: ppt 8��
#-----------------------------------------------------

agg_gender <- merge(aggregate(formula = �����ݾ� ~ ��ǰ���� + gender, data = potato , FUN = sum), count(potato, c("��ǰ����", "gender")), by = c("��ǰ����", "gender"))
agg_gender <- subset(agg_gender, subset = (gender != 0))
agg_gender$��ձ��ž� <- agg_gender$�����ݾ� / agg_gender$freq

## Plot
svg("./result/eda2_agg_gender.svg", width = 5, height = 5)
ggbase <- ggplot(agg_gender, aes(x = gender, y = ��ձ��ž�, color = ��ǰ����, group = ��ǰ����))
ggbase + geom_line(size = 2) + scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = paste0("��ǰ", c("A", "B", "C", "D"))) + ylab("") + xlab("") + scale_x_discrete(breaks = unique(agg_gender$gender), labels = c("����", "����")) + ylim(0, NA) +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

## ���Ǽ� test
sink("./result/eda2_test_gender.txt")
with(potato, by(potato, ��ǰ����, function(x) summary(lm(�����ݾ� ~ gender, data = x))))
sink()


#--------------------------------------------
# �ֺ� time-trend �׷��� ppt 9��
#--------------------------------------------

# ���� �� �ֺ�trend
svg("./result/eda2_sum_weekly.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = ��, y = �ֺ��հ�/10000, color = ��ǰ����))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

# ���� �� �ֺ�trend
potato_adjweek <- potato
potato_adjweek$������ <- potato_adjweek$�������� %/% 7 + 1
potato_adjweek <- merge(potato_adjweek, aggregate(�����ݾ� ~ ������ + ��ǰ����, data = potato_adjweek, FUN = sum), by = c("������", "��ǰ����"), all = TRUE)
colnames(potato_adjweek)[14] <- "�����ݾ�"
colnames(potato_adjweek)[19] <- "�����ֺ��հ�"

svg("./result/eda2_sum_adjweekly.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato_adjweek, aes(x = ������, y = �����ֺ��հ�/10000, color = ��ǰ����))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  # ggtitle("���� - �Ϸ� ��� ���űݾ�") +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

#--------------------------------------------
# �Ϻ� time-trend �׷��� ppt 10��
#--------------------------------------------

# ���� �� �Ϻ�trend
svg("./result/eda2_sum_daily.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = ���Ž���, y = �Ϻ��հ�/10000, color = ��ǰ����))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

# ���� �� �Ϻ�trend
svg("./result/eda2_sum_adjdaily.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato, aes(x = ��������, y = �Ϻ��հ�/10000, color = ��ǰ����))
ggbase + geom_line(size = 2) + 
  scale_color_discrete(name = "", breaks = paste0("��ǰ", c("A", "B", "C", "D")), labels = c("A", "B", "C", "D")) + ylab("") + xlab("") + ylim(0, NA) + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) +
  guides(colour = guide_legend(override.aes=list(size = 10)))
dev.off()

#--------------------------------------------
# ������ 4�� ������ ppt 10��
#--------------------------------------------
potato_daily <- unique(potato[, c("��ǰ����", "���Ž���", "�Ϻ��հ�")])
potato_list <- list() # to save each 
for(i in 1:4){
  potato_list[[LETTERS[i]]] <- potato_daily[which(potato_daily$��ǰ���� == paste0("��ǰ", LETTERS[i])),-1]
}
names(potato_list) <- LETTERS[1:4]
potato_list <- lapply(potato_list, function(x) x[order(x$���Ž���),])

SNR <- function(tsdata, num = 28){
  x <- tsdata[((length(tsdata) - num + 1):length(tsdata))]
  return(data.frame(mean = mean(x), sd = sd(x), snr = mean(x)/sd(x)))
}

## result
lapply(potato_list, function(x) SNR(x$�Ϻ��հ�))


#--------------------------------------------
# ��±׷��� ppt 11��
#--------------------------------------------

# ���vs�Ǹž�
potato_seoul <- subset(potato, �������� == "����")
potato_seoul$�Ϻ��հ� <- c() # remove
potato_seoul$�ֺ��հ� <- c() # remove
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("��ǰ����", "��������"), summarize, �Ϻ��հ� = sum(�����ݾ�)), by = c("��ǰ����", "��������"))
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("��ǰ����", "��"), summarize, �ֺ��հ� = mean(�Ϻ��հ�), �ֺ���ձ�� = mean(��ձ��)), by = c("��ǰ����", "��"))
potato_seoul <- unique(potato_seoul[,c("�ֺ��հ�", "��ǰ����", "��", "�ֺ���ձ��")])


## Plot
svg("./result/eda2_temperature_seoul.svg", width = 10, height = 8)
ggbase <- ggplot(data = potato_seoul, aes(x = �ֺ���ձ��, y = �ֺ��հ�/10000, group = ��ǰ����, color = ��ǰ����))
ggbase +
  geom_point() + 
  facet_wrap( ~ ��ǰ����, ncol = 2, scales = "free") + 
  xlab("") + ylab("") +
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) + guides(color = FALSE)
dev.off()

## correlation, ����� ���Ǽ� test
sink("./result/eda2_test_temperature.txt")
  with(potato_seoul, by(potato_seoul, ��ǰ����, function(x) cor(x$�ֺ��հ�, x$�ֺ���ձ��)))
  cat("\n\n")
  with(potato_seoul, by(potato_seoul, ��ǰ����, function(x) summary(lm(�ֺ��հ� ~ �ֺ���ձ��, data = x))))
sink()


# ��������vs���(����)
potato_seoul <- subset(potato, ((�������� == "����") & (��ǰ���� %in% c("��ǰA", "��ǰB"))))
potato_seoul$�Ϻ��հ� <- c() # remove
potato_seoul$�ֺ��հ� <- c() # remove
potato_seoul$������ <- potato_seoul$�������� %/% 7 + 1
potato_seoul <- merge(potato_seoul, ddply(potato_seoul, c("��ǰ����", "������"), summarize, �ֺ���ձ�� = mean(��ձ��)), by = c("��ǰ����", "������"))
potato_seoul <- unique(potato_seoul[,c("������", "�ֺ���ձ��")])

svg("./result/eda2_week_temperature.svg", width = 10, height = 4)
ggbase <- ggplot(potato_seoul, aes(x = ������, y = �ֺ���ձ��))
ggbase + geom_line() + xlab("") + ylab("") + 
  theme(plot.title = element_text(size = rel(6)),
        axis.text.x = element_text(size = rel(2)), 
        axis.text.y = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(3)), 
        legend.text = element_text(size = rel(2)), 
        legend.title = element_text(size = rel(4))) + guides(color = FALSE)
dev.off()



