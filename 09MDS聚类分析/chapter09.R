# File-Name:       chapter09.R
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 4.  In this case study we introduce multidimensional scaling (MDS),
#                   a technique for visually displaying the simialrity of observations in 
#                   mutlidimensional space.  We begin with with a very simple example using simulated
#                   data.  Next, we perform the same analysis on real data, using Senate roll call vote 
#                   data.  We first must get this data into workable format, then analyze with MDS
# Data Used:       *.dta files in code/data/, source: http://www.voteview.com/dwnl.htm
# Packages Used:   foreign, ggplot2

# All   source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

#多维定标技术(MDS)，基于观测值之间的距离度量进行聚类
#MDS:可视化研究参议员相似性  相似性聚类
getwd()
setwd("E:/workspace/R/machine learning/09-MDS")
# Load libraries
library('foreign')
library('ggplot2')

### Simulated analysis to review the technique

# Some sample code to understand matrix multiplication

# First code snippet.
set.seed(851982) # To make sure results are consistent
#用户(行),产品(列)
ex.matrix <- matrix(sample(c(-1, 0, 1), 24, replace = TRUE),
                    nrow = 4,
                    ncol = 6)
row.names(ex.matrix) <- c('A', 'B', 'C', 'D')
colnames(ex.matrix) <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')

# Second code snippet
ex.matrix

#P1 P2 P3 P4 P5 P6
#A 0 -1 0 -1 0 0
#B -1 0 1 1 1 0
#C 0 0 0 1 -1 1
#D 1 0 1 -1 0 0

# Convert matrix to customer-by-customer matrix

# Third code snippet
t(ex.matrix)

#A B C D
#P1 0 -1 0 1
#P2 -1 0 0 0
#P3 0 1 0 1
#P4 -1 1 1 -1
#P5 0 1 -1 0
#P6 0 0 1 0

# Fourth code snippet
# 计算距离矩阵
ex.mult <- ex.matrix %*% t(ex.matrix)
ex.mult

#A B C D
#A 2 -1 -1 1
#B -1 4 0 -1
#C -1 0 3 -1
#D 1 -1 -1 3

# 计算A与D用户之间的距离,欧式距离
sqrt(sum((ex.mult[1, ] - ex.mult[4, ]) ^ 2))
#[1] 2.236068


#计算距离，dist有很多种不同的计算方法，默认下三角
ex.dist <- dist(ex.mult)
ex.dist

#A B C
#B 6.244998
#C 5.477226 5.000000
#D 2.236068 6.782330 6.082763

#cmdscale默认在二维空间中计算MDS
ex.mds <- cmdscale(ex.dist)
plot(ex.mds, type = 'n')
text(ex.mds, c('A', 'B', 'C', 'D')) #将字符映射到图坐标上

################################################################
# 对美国参议员做聚类
library('foreign')
library('ggplot2')

data.dir <- file.path("data", "roll_call")
#列出路径下所的文件名
data.files <- list.files(data.dir)
#dta是stata文件
data.files

#[1] "sen101kh.dta" "sen102kh.dta"
#[3] "sen103kh.dta" "sen104kh.dta"
#[5] "sen105kh.dta" "sen106kh.dta"
#[7] "sen107kh.dta" "sen108kh_7.dta"
#[9] "sen109kh.dta" "sen110kh_2008.dta"
#[11] "sen111kh.dta"

#将11个数据框文件放入rollcall.data中，每个数据文件代表每一届的投票情况
#lapply输入的向量,取每个元素，放入function中，返回一个列表
rollcall.data <- lapply(data.files,
                        function(f)
                        {
                          read.dta(file.path(data.dir, f), convert.factors = FALSE)
                        })
class(rollcall.data)
# Ninth code snippet
dim(rollcall.data[[1]])
#[1] 103 647
#首先每一行对于一位投票者，其次数据库前9列包含投票者的身份信息，剩余的是实际投票
head(rollcall.data[[1]])
#cong id state dist lstate party eh1 eh2 name V1 V2 V3 ... V638
#1 101 99908 99 0 USA 200 0 0 BUSH 1 1 1 ... 1
#2 101 14659 41 0 ALABAMA 100 0 1 SHELBY, RIC 1 1 1 ... 6
#3 101 14705 41 0 ALABAMA 100 0 1 HEFLIN, HOW 1 1 1 ... 6
#4 101 12109 81 0 ALASKA 200 0 1 STEVENS, TH 1 1 1 ... 1
#5 101 14907 81 0 ALASKA 200 0 1 MURKOWSKI, 1 1 1 ... 6
#6 101 14502 61 0 ARIZONA 100 0 1 DECONCINI, 1 1 1 ... 6

# 构建参议员-投票矩阵
#首先删除state列中等于99的观测值，编码99是对应州的副总统的投票，而副总统很少投票
#故将其删除；
#将无效投票（大于6）设置为0
#将赞成票（0-4）设置为1
#将反对票（大于4）设置为-1
rollcall.simplified <- function(df)
{
  no.pres <- subset(df, state < 99)
  
  for(i in 10:ncol(no.pres))
  {
    no.pres[,i] <- ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i] > 4, -1, no.pres[,i])
  }
  
  return(as.matrix(no.pres[,10:ncol(no.pres)]))
}

#lapply 输入列表，取每个列表，放入function中，返回列表
#将票数，转化为了赞成 反对和弃权三类
rollcall.simple <- lapply(rollcall.data, rollcall.simplified)

#对于分类数据，采用距离矩阵进行聚类，m是投票的分类数据的矩阵，
#用dist求分类数据的距离
rollcall.dist <- lapply(rollcall.simple, function(m) dist(m %*% t(m)))

# k=2表示二维空间计算MDS
rollcall.mds <- lapply(rollcall.dist,
                       function(d) as.data.frame((cmdscale(d, k = 2)) * -1))

#将投票者身份信息加到rollcall.mds的坐标点数据框中
congresses <- 101:111
#对11届投票循环，取出每个届的mds坐标点数据；两个中括号取的是列表
#sapply同lapply一样，但是它能得到向量或矩阵，
#strsplit("SHELBY, RIC", "[, ]")[[1]][1] 中[, ]是因为congress$name的一个字符串中
#两个字符之间有逗号+空格， 该函数返回一个列表，[[1]]取列表1， [1]该列表下第一个字符
#
for(i in 1:length(rollcall.mds))
{
  names(rollcall.mds[[i]]) <- c("x", "y")
  
  congress <- subset(rollcall.data[[i]], state < 99)

  congress.names <- sapply(as.character(congress$name),
                           function(n) strsplit(n, "[, ]")[[1]][1])
  
  rollcall.mds[[i]] <- transform(rollcall.mds[[i]],
                                 name = congress.names,
                                 party = as.factor(congress$party),
                                 congress = congresses[i])
}

#第一届的数据
head(rollcall.mds[[1]])

#x y name party congress
#2 -11.44068 293.0001 SHELBY 100 101
#3 283.82580 132.4369 HEFLIN 100 101
#4 885.85564 430.3451 STEVENS 200 101
#5 1714.21327 185.5262 MURKOWSKI 200 101
#6 -843.58421 220.1038 DECONCINI 100 101
#7 1594.50998 225.8166 MCCAIN 200 101

# Thirteenth code snippet
# Create a plot of just the 110th Congress
cong.110 <- rollcall.mds[[9]]

#scale_size 画布大小；scale_alpha画布透明度， theme_bw()背景；theme()主题
#scale_shape
#scale_color_manual
base.110 <- ggplot(cong.110, aes(x = x, y = y)) +
  scale_size(range = c(2,2), guide = 'none') +
  scale_alpha(guide = 'none') +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggtitle("Roll Call Vote MDS Clustering for 110th U.S. Senate") +
  xlab("") +
  ylab("") +
  scale_shape(name = "Party", breaks = c("100", "200", "328"),
              labels = c("Dem.", "Rep.", "Ind."), solid = FALSE) +
  scale_color_manual(name = "Party", values = c("100" = "black",
                                                "200" = "dimgray",
                                                "328"="grey"),
                     breaks = c("100", "200", "328"),
                     labels = c("Dem.", "Rep.", "Ind."))

#以不同党派对应不同的形状代表数据点画图
#shaple图形形状，size图形大小
print(base.110 + geom_point(aes(shape = party,
                                alpha = 0.75,
                                size = 2)))
#以参议员名字代表数据点
#color 党派颜色，alpha图形透明度，lable图例，size字符大小
print(base.110 + geom_text(aes(color = party,
                               alpha = 0.75,
                               label = cong.110$name,
                               size = 2)))

# Fourteenth code snippet
# do.call将列表通过rbind 压缩进一个数据框中，通过分层画图，来展示参议员投票的聚类情况
all.mds <- do.call(rbind, rollcall.mds)
all.plot <- ggplot(all.mds, aes(x = x, y = y)) +
  geom_point(aes(shape = party, alpha = 0.75, size = 2)) +
  scale_size(range = c(2, 2), guide = 'none') +
  scale_alpha(guide = 'none') +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggtitle("Roll Call Vote MDS Clustering for U.S. Senate (101st - 111th Congress)") +
       xlab("") +
       ylab("") +
       scale_shape(name = "Party",
                   breaks = c("100", "200", "328"),
                   labels = c("Dem.", "Rep.", "Ind."),
                   solid = FALSE) +
      facet_wrap(~ congress)

print(all.plot)

#画出每一届投票的图 
for(i in 1:length(rollcall.mds))
{
  mds <- rollcall.mds[[i]]
  congress <- congresses[i]
  plot.title <- paste("Roll Call Vote MDS Clustering for ",
                      congress,
                      " U.S. Senate",
                      sep = "")
  
  #构造画布
  mds.plot <- ggplot(mds, aes(x = x, y = y)) +
    scale_size(range = c(2, 2), guide = 'none') +
    scale_alpha(guide = 'none') +
    theme_bw() +
    theme(axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank()) +
    ggtitle(plot.title) +
    xlab("") +
    ylab("")
  
  # 在画布上画图
  mds.point <- mds.plot + geom_point(aes(shape = party,
                                         alpha = 0.75,
                                         size = 2))
  mds.text <- mds.plot + geom_text(aes(color = party,
                                       alpha = 0.75,
                                       label = mds$name,
                                       size = 2))
  
  # Fix labels, shapes and colors
  if(length(levels(mds$party)) > 2)
  {
    mds.point <- mds.point + scale_shape(name = "Party",
                                         breaks = c("100", "200", "328"),
                                         labels = c("Dem.", "Rep.", "Ind."),
                                         solid = FALSE)
    mds.text <- mds.text + scale_color_manual(name = "Party",
                                              values = c("100" = "black",
                                                         "200" = "dimgray",
                                                         "328" = "gray"),
                                              breaks = c("100", "200", "328"),
                                              labels = c("Dem.", "Rep.", "Ind."))
  }
  else
  {
    mds.point <- mds.point + scale_shape(name = "Party",
                                         breaks = c("100", "200"),
                                         labels = c("Dem.", "Rep."),
                                         solid = FALSE)
    mds.text <- mds.text + scale_color_manual(name = "Party",
                                              values = c("100" = "black",
                                                         "200" = "dimgray"),
                                              breaks = c("100", "200"),
                                              labels = c("Dem.", "Rep."))
  }
  #以名字保存图片
  ggsave(plot = mds.point,
         filename = file.path('images',
                              'senate_plots',
                              paste(congress, "_point.pdf", sep = "")),
         width = 8,
         height = 5)
  ggsave(plot = mds.text,
         filename = file.path('images',
                              'senate_plots',
                              paste(congress, "_names.pdf", sep = "")),
         width = 8,
         height = 5)
}
