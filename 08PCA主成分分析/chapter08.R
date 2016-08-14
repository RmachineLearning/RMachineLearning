# File-Name:       chapter08.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/DJI.csv, data/stock_prices.csv
# Packages Used:   ggplot2, lubridate, reshape

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

#主成分分析
setwd("E:/workspace/R/machine learning/08-PCA")
library('ggplot2')

# First code snippet
prices <- read.csv(file.path('data', 'stock_prices.csv'),
                   stringsAsFactors = FALSE)

head(prices)
# Date Stock Close
#1 2011-05-25 DTE 51.12

# lubridate包中ymd()函数能够把“年-月-日”这种格式的字符串转换成日期对象
#install.packages('lubridate')
library('lubridate')
prices <- transform(prices, Date = ymd(Date))

# reshape包中的cast()函数,将price堆积数据，分隔成行代表时间Date，列代表股票stock
#close表示每一个元素的取值
library('reshape')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')
head(date.stock.matrix)
# 发现date.stock.matrix中含有缺失值，所以我们要先删除缺失的数据，在cast()
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

#考察所有股票的相关性，然后把相关系数矩阵转化成一个数值向量，并画一个相关性密度图
#为的是获得两个直观的认识：1 相关性的均值；2 低相关性出现的频率
cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

#通过画密度图，看这些相关系数的分布，适不适合做PCA
#从图中看出大部分相关性是正的，因此PCA适用这份数据
ggplot(data.frame(Correlation = correlations),
  aes(x = Correlation, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# 主成分分析
#从汇总中，标准差告诉我们每一个主成分的贡献率(成分方差占总方差的比例)是多少
#第一主成分comp.1 29% com.2 20%,可以看出采用第一主成分就能很好对数据进行学习
pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])
summary(pca)


# 查看第一主成分载荷(loading),载荷告诉我们它给第一主成分多大权重
#因为只对第一主成分感兴趣，所以去第一主成分载荷
principal.component <- pca$loadings[, 1]

# 用密度图了解第一主成分是如何形成的
loadings <- as.numeric(principal.component)
#从图中可以看出，载荷几乎都是负数，
ggplot(data.frame(Loading = loadings),
  aes(x = Loading, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')



#我们已经获得了主成分，接下来可以把数据总结成一列，使用predict()
#第一主成分，代表一个市场指数
market.index <- predict(pca)[, 1]

# 为了检验主成分分析的效果，把主成分分析预测结果与股票市场指数进行对比，使用道琼斯指数
dji.prices <- read.csv(file.path('data', 'DJI.csv'),
                       stringsAsFactors = FALSE)
dji.prices <- transform(dji.prices, Date = ymd(Date))

# Twelfth code snippet
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

# DJI中我们感兴趣的部分是每日收盘价和纪录的日期，由于数据相反用rev()翻转数据
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# 使用PCA生成的市场指数market.index与DJI相比
comparison <- data.frame(Date = dates,
                         MarketIndex = market.index,
                         DJI = dji)

#从图中可以看出，主成分指标与道琼斯指标呈负相关，
#这是由于主成分指标的载荷都是负的原因
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# 通过乘以-1 将指标转变过来
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)

# 再次画图，得到的是正相关的
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Seventeenth code snippet
alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison,
       aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

###修正了指数方向，下面就是获得我们的指数随着时间的推移与DJI的趋势保持一致
#首先对两个指标标准化
comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, DJI = scale(DJI))
head(comparison)
#然后用melt函数，将两个指标融合在一起,这样k一次性可以对两个指标进行可视化
#当两个变量在一个数据框中，用melt就会将两个变量堆积起来
alt.comparison <- melt(comparison, id.vars = 'Date')
head(alt.comparison)
tail(alt.comparison)

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()
