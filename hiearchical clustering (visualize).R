# 해당 코드는 R 버전 4.1.1에서 작성되었다.
# 본 논문의 통계 분석에서 활용한 코드와 데이터는 다음 웹페이지에서 받을 수 있다.
# https://github.com/danielchoi5634/Replication-Data-for-Thesis

library(proxy) # version 0.4-26
# setwd("C:/File Location/")

########## 제1기 ###########
d1 <- read.csv("제1기 2-mode.csv", header=TRUE)
d1[is.na(d1)] <- 0
discourse1.remove.na <- d1[rowSums(d1[, 2:45])>0, ]

hc1 <- hclust(dist(discourse1.remove.na[2:45], method="Jaccard"), method="average")
plot(hc1, hang=-1, labels = discourse1.remove.na$actor, cex = 0.7,
     ylab="군집 간 평균 자카드 거리")
rect.hclust(hc1, h=0.95, border="purple")

########## 제2기 ###########
d2 <- read.csv("제2기 2-mode.csv", header=TRUE)
d2[is.na(d2)] <- 0
discourse2.remove.na <- d2[rowSums(d2[, 2:45])>0, ]

hc2 <- hclust(dist(discourse2.remove.na[2:45], method="Jaccard"), method="average")
plot(hc2, hang=-1, labels = discourse2.remove.na$actor, cex = 0.7,
     ylab="군집 간 평균 자카드 거리")
rect.hclust(hc2, h=0.95, border="purple")

########## 제3기 ###########
d3 <- read.csv("제3기 2-mode.csv", header=TRUE)
d3[is.na(d3)] <- 0
discourse3.remove.na <- d3[rowSums(d3[, 2:45])>0, ]

hc3 <- hclust(dist(discourse3.remove.na[2:45], method="Jaccard"), method="average")
plot(hc3, hang=-1, labels = discourse3.remove.na$actor, cex = 0.7,
     ylab="군집 간 평균 자카드 거리")
rect.hclust(hc3, h=0.95, border="purple")