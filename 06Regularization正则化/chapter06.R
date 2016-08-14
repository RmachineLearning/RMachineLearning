# File-Name:       chapter06.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/oreilly.csv
# Packages Used:   ggplot2, glmnet, tm, boot

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!
#第6章 正则化：文本回归
setwd("E:/workspace/R/machine learning/06-Regularization")
library('ggplot2')

# 数据列之间的非线性关系
set.seed(1)

x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)
#为了对数据进行光滑非线性拟合，可用GAM广义可加模型，
#geom_smooth(se = FALSE)使用默认的method参数就可以拟合GAM模型
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + 
  geom_point() +
  geom_smooth(se = FALSE)

# Second code snippet
x.squared <- x ^ 2

# Third code snippet
ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Fourth code snippet
summary(lm(y ~ x))$r.squared
#[1] 2.973e-06

summary(lm(y ~ x.squared))$r.squared
#[1] 0.9707

# Fifth code snippet
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

ggplot(df, aes(x = X, y = Y)) +
  geom_point()

# Sixth code snippet
summary(lm(Y ~ X, data = df))

#Call:
#lm(formula = Y ~ X, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-1.00376 -0.41253 -0.00409 0.40664 0.85874
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.94111 0.09057 10.39 <2e-16 ***
#X -1.86189 0.15648 -11.90 <2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.4585 on 99 degrees of freedom
#Multiple R-squared: 0.5885, Adjusted R-squared: 0.5843
#F-statistic: 141.6 on 1 and 99 DF, p-value: < 2.2e-16

# Seventh code snippet
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# 将一个复杂的非线性问题转化为线性问题时机器学习常见的做法，例如核方法
#多项式方法等
df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)

summary(lm(Y ~ X + X2 + X3, data = df))

#Call:
#lm(formula = Y ~ X + X2 + X3, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.32331 -0.08538 0.00652 0.08320 0.20239
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -0.16341 0.04425 -3.693 0.000367 ***
#X 11.67844 0.38513 30.323 < 2e-16 ***
#X2 -33.94179 0.89748 -37.819 < 2e-16 ***
#X3 22.59349 0.58979 38.308 < 2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.1153 on 97 degrees of freedom
#Multiple R-squared: 0.9745, Adjusted R-squared: 0.9737
#F-statistic: 1235 on 3 and 97 DF, p-value: < 2.2e-16

# Ninth code snippet
df <- transform(df, X4 = X ^ 4)
df <- transform(df, X5 = X ^ 5)
df <- transform(df, X6 = X ^ 6)
df <- transform(df, X7 = X ^ 7)
df <- transform(df, X8 = X ^ 8)
df <- transform(df, X9 = X ^ 9)
df <- transform(df, X10 = X ^ 10)
df <- transform(df, X11 = X ^ 11)
df <- transform(df, X12 = X ^ 12)
df <- transform(df, X13 = X ^ 13)
df <- transform(df, X14 = X ^ 14)
df <- transform(df, X15 = X ^ 15)

summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14,
           data = df))

#Call:
#lm(formula = Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 +
# X10 + X11 + X12 + X13 + X14, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.242662 -0.038179 0.002771 0.052484 0.210917
#
#Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -6.909e-02 8.413e-02 -0.821 0.414
#X 1.494e+01 1.056e+01 1.415 0.161
#X2 -2.609e+02 4.275e+02 -0.610 0.543
#X3 3.764e+03 7.863e+03 0.479 0.633
#X4 -3.203e+04 8.020e+04 -0.399 0.691
#X5 1.717e+05 5.050e+05 0.340 0.735
#X6 -6.225e+05 2.089e+06 -0.298 0.766
#X7 1.587e+06 5.881e+06 0.270 0.788
#X8 -2.889e+06 1.146e+07 -0.252 0.801
#X9 3.752e+06 1.544e+07 0.243 0.809
#X10 -3.398e+06 1.414e+07 -0.240 0.811
#X11 2.039e+06 8.384e+06 0.243 0.808
#X12 -7.276e+05 2.906e+06 -0.250 0.803
#X13 1.166e+05 4.467e+05 0.261 0.795
#X14 NA NA NA NA
#
#Residual standard error: 0.09079 on 87 degrees of freedom
#Multiple R-squared: 0.9858, Adjusted R-squared: 0.9837
#F-statistic: 465.2 on 13 and 87 DF, p-value: < 2.2e-16

# 多项式拟合回归poly()创建正交多项式，变量过度会造成过度拟合
summary(lm(Y ~ poly(X, degree = 14), data = df))

#Call:
#lm(formula = Y ~ poly(X, degree = 14), data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.232557 -0.042933 0.002159 0.051021 0.209959
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.010167 0.009038 1.125 0.2638
#poly(X, degree = 14)1 -5.455362 0.090827 -60.063 < 2e-16 ***
#poly(X, degree = 14)2 -0.039389 0.090827 -0.434 0.6656
#poly(X, degree = 14)3 4.418054 0.090827 48.642 < 2e-16 ***
#poly(X, degree = 14)4 -0.047966 0.090827 -0.528 0.5988
#poly(X, degree = 14)5 -0.706451 0.090827 -7.778 1.48e-11 ***
#poly(X, degree = 14)6 -0.204221 0.090827 -2.248 0.0271 *
#poly(X, degree = 14)7 -0.051341 0.090827 -0.565 0.5734
#poly(X, degree = 14)8 -0.031001 0.090827 -0.341 0.7337
#poly(X, degree = 14)9 0.077232 0.090827 0.850 0.3975
#poly(X, degree = 14)10 0.048088 0.090827 0.529 0.5979
#poly(X, degree = 14)11 0.129990 0.090827 1.431 0.1560
#poly(X, degree = 14)12 0.024726 0.090827 0.272 0.7861
#poly(X, degree = 14)13 0.023706 0.090827 0.261 0.7947
#poly(X, degree = 14)14 0.087906 0.090827 0.968 0.3358
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.09083 on 86 degrees of freedom
#Multiple R-squared: 0.986, Adjusted R-squared: 0.9837
#F-statistic: 431.7 on 14 and 86 DF, p-value: < 2.2e-16

# Eleventh code snippet
poly.fit <- lm(Y ~ poly(X, degree = 1), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 3), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 5), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 25), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

##############################################################
# 避免过度拟合的方法：
#通过交叉验证帮助选择多项式回归的次数
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# Thirteenth code snippet
n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))
#构建训练集
training.x <- x[indices]
training.y <- y[indices]

#构建测试集
test.x <- x[-indices]
test.y <- y[-indices]

#训练集数据框
training.df <- data.frame(X = training.x, Y = training.y)
#测试集数据框
test.df <- data.frame(X = test.x, Y = test.y)

# 计算RMSE函数
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# 将多次交叉验证(验证次数)的RMSE数据放入其中
#训练集和测试集的预测最接近时取得最佳的degree
performance <- data.frame()

for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                                              newdata = test.df))))
}
head(performance)
# rbind()数据排列是为了方便画图对比
#当次数过低时，拟合效果差，叫欠拟合
#次数过高，拟合效果差，过拟合
#随着次数不断增多，训练误差和测试误差趋势开始不一致，训练集误差持续减少，测试误差开始变大
#这就是过拟合
ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

###############################################################
# 为了避免过拟合，使用正则化

#通过累加返回的coef值，来度量模型的复杂度
lm.fit <- lm(y ~ x)
model.complexity <- sum(coef(lm.fit) ^ 2)

# 不希望累加时正负抵消，用L2正则化取平方， L1正则化取绝对值
lm.fit <- lm(y ~ x)
l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

#我么要让模型尽量拟合训练数据，也要让模型尽量简单
#通过正则化我们就能够避免模型去拟合那些训练集数据中的噪声，进而避免过拟合
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# Twentieth code snippet
x <- as.matrix(cbind(x,rev(x)))

#该包提供了训练正则化的线性模型的函数glmnet()
library('glmnet')
glmnet(x, y)
#DF指明模型中非零权重有几个， 但不包含截距项，如果模型把某些输入特征的权重设为0，
#仍然有比较好的效果，那么我们就认为那些权重为0的输入特征(变量)时无关紧要的。
#当大部分输入特征的权重是0，那么这个模型是稀疏模型
#%Dev 其实就是模型R^2的值，第一行为0%是因为你把唯一输入特征权重设置为0，所以这个
#模型实质是一个常数模型
#最后%DEV 是59%这个值和你直接使用lm函数得到的R^2的值时一样的，因为lm没有正则化
#Lambda是正则化算法的参数，控制拟合模型的复杂度，lambda很大，对复杂模型惩罚严重
#迫使模型权重趋近0，lambda小对模型复杂度不关心，不怎么惩罚
#用交叉验证的方法，得到最佳的lambda
#Call: glmnet(x = x, y = y)
#
# Df %Dev Lambda
# [1,] 0 0.00000 0.542800
# [2,] 1 0.09991 0.494600
# [3,] 1 0.18290 0.450700
# [4,] 1 0.25170 0.410600
# [5,] 1 0.30890 0.374200
#...
#[51,] 1 0.58840 0.005182
#[52,] 1 0.58840 0.004721
#[53,] 1 0.58850 0.004302
#[54,] 1 0.58850 0.003920
#[55,] 1 0.58850 0.003571

# 交叉验证得到最佳lambda
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# glmnet()用训练集得到多个lambda，然后带入到测试集中看预测的RMSE效果
library('glmnet')

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))

lambdas <- glmnet.fit$lambda
#可以设为空数据框
performance <- data.frame()

for (lambda in lambdas)
{
  performance <- rbind(performance,
                       data.frame(Lambda = lambda,
                                  RMSE = rmse(test.y,
                                              with(test.df,
                                                   predict(glmnet.fit,
                                                           poly(X, degree = 10),
                                                           s = lambda)))))
}

# Twenty-third code snippet
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()

#对x轴做log10的尺度变换
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  scale_x_log10()

#找出最小的RMSE，从而找到最小的lambda
best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])

glmnet.fit <- with(df, glmnet(poly(X, degree = 10), Y))

# Twenty-fifth code snippet
coef(glmnet.fit, s = best.lambda)

#11 x 1 sparse Matrix of class "dgCMatrix"
# 1
#(Intercept) 0.0101667
#1 -5.2132586
#2 0.0000000
#3 4.1759498
#4 0.0000000
#5 -0.4643476
#6 0.0000000
#7 0.0000000
#8 0.0000000
#9 0.0000000
#10 0.0000000

################################################################
#文本回归
#交叉验证和正则化是两个非常强大的工具，当使用文本作为回归模型的输入值时，
#输入特征个数(单词个数或者变量个数)比样本数(文档)还要多，所以非正则化的回归总是
#过拟合的，必须用正则化手段
ranks <- read.csv(file.path('data', 'oreilly.csv'),
                  stringsAsFactors = FALSE)
#oreilly.csv包含的是oreilly出版社前100的畅销书，封底描述文本来预测这些书相对流行程度
#IP_Family 书地址 BOOK.title书名  BOOK.ISBN书号 Rank排名 Long.Desc 封底对书的描述
#
head(ranks[,c(1,3)])
names(ranks)
#正则化包，利用tm包将原始数据集转化成为一个文档词项矩阵
library('tm')

documents <- data.frame(Text = ranks$Long.Desc.)
#每本书的描述，分别命名为1到最后
row.names(documents) <- 1:nrow(documents)

#DataframeSource()使数据框每一行代表一个文本
#然后基于数据框创建一个语料库(corpus)，
corpus <- Corpus(DataframeSource(documents))
#tm_map()基于corpus进行文本转换，将文本中的单词全部统一成小写
#corpus <- tm_map(corpus, tolower)
#去除多余空格
corpus <- tm_map(corpus, stripWhitespace)
#删除英语中常用的停用词
corpus <- tm_map(corpus, removeWords, stopwords('english'))
#创建文档词项矩阵
dtm <- DocumentTermMatrix(corpus)


# 排名第一的是100 排名100的是1
#因为每本书的封面描述所用单词不一样，所以很多单词权重为0
x <- as.matrix(dtm)
y <- rev(1:100)

# Twenty-eighth code snippet
set.seed(1)

library('glmnet')

# Twenty-ninth code snippet
performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5))
{
  for (i in 1:50)
  {
    indices <- sample(1:100, 80) #100中随机抽取80个数
    
    #构建训练集
    training.x <- x[indices, ] #每个文本单词
    training.y <- y[indices]   #每本书的排名得分
    #测试集
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    
    #x矩阵的行为文本，列为单词
    glm.fit <- glmnet(training.x, training.y)
    #预测文本的排名
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    
    #计算测试集标准均方误
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    RMSE = rmse))
  }
}

#从图中可以看出，lambda越大，模型表现越好，这样会简化到一个常数模型
#这说明它没有任何文本信息，简而言之我们的文本回归模型没有
#发现任何有意义的信息，因它给出的预测完全是随机噪声
#所以说保证书畅销的“神奇单词”是不存在的
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

########################################################
#用逻辑回归帮忙
#虽然不能创建一个 文本预测工具，但是可以预测某书是否进入销售排名前50
#如果进入前50，则表示为1 否则为0
y <- rep(c(1, 0), each = 50)

# family控制了预测的误差服从的分布类型
regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-third code snippet
regularized.fit <- glmnet(x, y)

regularized.fit <- glmnet(x, y, family = 'gaussian')

regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-fourth code snippet
predict(regularized.fit, newx = x, s = 0.001)
#1 4.884576
#2 6.281354
#3 4.892129
#...
#98 -5.958003
#99 -5.677161
#100 -4.956271

# 由于输出的是正数和负数不是0和1，有两种方法来处理
#1 以0为阀值，作出0 1 预测
#2 把原始预测输出转换成我们容易理解的概率值
ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)
#1 1
#2 1
#3 1
#...
#98 0
#99 0
#100 0

#为了把逻辑回归的原始预测值转化成概率，要用到boot包了的inv.logit函数
library('boot')
#该函数是逻辑回归模型的输出需要经过逆logit函数才能变成概率输出
inv.logit(predict(regularized.fit, newx = x, s = 0.001))
#1 0.992494427
#2 0.998132627
#3 0.992550485
#...
#98 0.002578403
#99 0.003411583
#100 0.006989922

# 下面的的代码与前面用来预测排名的线性回归代码  相差不大
set.seed(1)

performance <- data.frame()

for (i in 1:250)
{
  indices <- sample(1:100, 80)
  
  training.x <- x[indices, ]
  training.y <- y[indices]
  
  test.x <- x[-indices, ]
  test.y <- y[-indices]
  
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    
    error.rate <- mean(predicted.y != test.y)

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}

# Thirty-eighth code snippet
ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
  scale_x_log10()
