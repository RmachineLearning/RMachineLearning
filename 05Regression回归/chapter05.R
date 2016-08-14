# File-Name:       chapter05.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/longevity.csv
# Packages Used:   ggplot2

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

#
setwd("/media/zhoutao/软件盘/workspace/R/machine learning/05-Regression")
setwd("E:/workspace/R/machine learning/05-Regression")
library('ggplot2')

# 当有多个文件数据时，用该方法读取数据
#Smokes是否吸烟 AgeAtDeath死亡年龄
ages <- read.csv(file.path('data', 'longevity.csv'))
head(ages)
#facet_grid()分面
ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
  geom_density() +
  facet_grid(Smokes ~ .)


#AgeAtDeath死亡平均年龄是72.7,如果假设每个人都活到73岁，那么我们对于上述数据集中的
#人们的年龄所作出的平均寿命为73岁这个预测有多差
#用均方误差MSE来度量
ages <- read.csv(file.path('data', 'longevity.csv'))
guess <- 73
with(ages, mean((AgeAtDeath - guess) ^ 2))
#[1] 32.991


#下面验证平均寿命为73岁的假设是非常好的结果
#取63到83之间的数作为平均寿命的假设，看一看哪个平均值的均方误差最小
ages <- read.csv(file.path('data', 'longevity.csv'))
guess.accuracy <- data.frame()

for (guess in seq(63, 83, by = 1))
{
  prediction.error <- with(ages,
                           mean((AgeAtDeath - guess) ^ 2))
  #下面告诉我们如何在数据框中追加数据
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                     Error = prediction.error))
}

#课件均值73是最好的预测值
ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
  geom_point() +
  geom_line()

###################################################
#使用虚拟变量的回归模型
#吸烟和不吸烟对死亡的影响
ages <- read.csv(file.path('data', 'longevity.csv'))

constant.guess <- with(ages, mean(AgeAtDeath))
#不分吸烟和吸烟者信息，计算出来的标准均方误
with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))
#吸烟者平均寿命
smokers.guess <- with(subset(ages, Smokes == 1),
                      mean(AgeAtDeath))
#不吸烟者平均寿命
non.smokers.guess <- with(subset(ages, Smokes == 0),
                          mean(AgeAtDeath))
#在age中增加一列，记录吸烟和不吸烟者的各自的平均寿命
ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                         non.smokers.guess,
                                         smokers.guess))
head(ages)
#考虑了吸烟者的信息，计算出来的均方误
#两个均方误对比发现，考虑吸烟者信息能降低标准均方误，
#因此二元区分，对我们的预测是有影响的
with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))

##########################################################
#我们要将虚拟变量整合进预测机制中，为了使用好数据要考虑
#1 要知道如何使用不是二元区分性输入，例如身高 体重的连续型值
#2 如何一次性使用多重信息源以提升预测
library('ggplot2')

heights.weights <- read.csv(file.path('data',
                                      '01_heights_weights_genders.csv'),
                            header = TRUE,
                            sep = ',')

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Sixth snippet
fitted.regression <- lm(Weight ~ Height,
                        data = heights.weights)

coef(fitted.regression)
#(Intercept) Height
#-350.737192 7.717288

# Seventh snippet
intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]

# predicted.weight <- intercept + slope * observed.height
# predicted.weight == -350.737192 + 7.717288 * observed.height
#计算预测值
predict(fitted.regression)

# 计算误差
true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)
residuals(fitted.regression)

# 把残差和真实数据对应的画在一幅图中
#which = 1是让R只画出第一个回归诊断图
plot(fitted.regression, which = 1)

# Twelfth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

plot(fitted.regression, which = 1)

# Thirteenth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
sum(squared.errors)
#[1] 528

# 误差衡量指标是：1 取所有残差 2 对它们进行平方处理，以取得模型平方误差
#3 把这些平方误差加在一起
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
mse
#[1] 52.8

#误差平方和在大数据上的值比在小数据上的值大，这一点非常不好
#所以通常用误差平方的均值(MSE)，而不是它们的和作为评价标准
#然而MSE是通过平方得到的，会放大误差，所以用均方根误差（RMSE）来衡量
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse
#[1] 7.266361

#RMSE不能直接让人看出哪个模型表现情况，所以用R^2来判断
#R-squared 的思路是：要看你的模型与假如只是用均值作为预测结果相比有多好
#计算R^2需要计算两个RMSE
#第一步是只使用均值来当所有样本数据的预测值时的RMSE
#第二部是使用你的模型所作出的预测RMSE
# R-squared is a ratio of MSE's, not RMSE's.
mean.mse <- 1.09209343
model.mse <- 0.954544

r2 <- 1 - (model.mse / mean.mse)
r2
#[1] 0.1259502

########################################################################
#预测网页流量
top.1000.sites <- read.csv(file.path('data', 'top_1000_sites.tsv'),
                           sep = '\t',
                           stringsAsFactors = FALSE)
#Rank 网站流量的排名； 
#Site网站名；  
#Category   
#UniqueVisitors采集样本的月份有多少不同用户在访问这个网站  
#PageViews  网站访问次数；  
#HasAdvertising 网站上是否有广告
#InEnglish   网站主要语言是不是英语
#TLD
head(top.1000.sites)
ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) +
  geom_point()
ggsave(file.path("images", "page_views_vs_visitors.pdf"))

# Eighteenth snippet
ggplot(top.1000.sites, aes(x = PageViews)) +
  geom_density()

# 取对数重画该图
ggplot(top.1000.sites, aes(x = log(PageViews))) +
  geom_density()

# Twentieth snippet
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point()
ggsave(file.path("images", "log_page_views_vs_log_visitors.pdf"))

# Twenty-first snippet
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
ggsave(file.path("images", "log_page_views_vs_log_visitors_with_lm.pdf"))

# Twenty-second snippet
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)

# log(UniqueVisitors)中的 Estimate =1.33628 Std. Error = 0.04568
#那么这个系数与零距离(1.33628/0.04568 =) 29.25306个标准差
#如果得到的系数与零距离在3个标准差之上，说明两个变量之间是相关的，不符合
#变量间独立的假设条件
#t value是检验系数估计值距离零的标准差个数
# 标准均方误RMES = sqrt(mean(residuals(lm.fit)^2))
#自由度：有1000个样本，2个因变量，所以自由度为998，如果有500个因变量去拟合1000个
#样本，RMSE再小也没有意义，这叫过度拟合
#F-statistic 是R^2的一个替代方案，用来计算p—value，p—value具有欺骗性，所以一般
#不太相信F-statistic
summary(lm.fit)

#Call:
#lm(formula = log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
#
#Residuals:
# Min 1Q Median 3Q Max
#-2.1825 -0.7986 -0.0741 0.6467 5.1549
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -2.83441 0.75201 -3.769 0.000173 ***
#log(UniqueVisitors) 1.33628 0.04568 29.251 < 2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.084 on 998 degrees of freedom
#Multiple R-squared: 0.4616, Adjusted R-squared: 0.4611
#F-statistic: 855.6 on 1 and 998 DF, p-value: < 2.2e-16

# Twenty-fourth snippet
lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
             data = top.1000.sites)
summary(lm.fit)

#Call:
#lm(formula = log(PageViews) ~ HasAdvertising + log(UniqueVisitors) +
# InEnglish, data = top.1000.sites)
#
#Residuals:
# Min 1Q Median 3Q Max
#-2.4283 -0.7685 -0.0632 0.6298 5.4133
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -1.94502 1.14777 -1.695 0.09046 .
#HasAdvertisingYes 0.30595 0.09170 3.336 0.00088 ***
#log(UniqueVisitors) 1.26507 0.07053 17.936 < 2e-16 ***
#InEnglishNo 0.83468 0.20860 4.001 6.77e-05 ***
#InEnglishYes -0.16913 0.20424 -0.828 0.40780
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.067 on 995 degrees of freedom
#Multiple R-squared: 0.4798, Adjusted R-squared: 0.4777
#F-statistic: 229.4 on 4 and 995 DF, p-value: < 2.2e-16

# HasAdvertising是否有广告，作为因子进入模型，因子的一种情况(无广告)会记入截距中，
#一种情况（有广告）没有计入截距中
#回归中，截距就是对无广告且log(HasAdvertising)值为零的网站的流量的预测
lm.fit <- lm(log(PageViews) ~ HasAdvertising,
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.01073766

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.4615985

#InEnglish因子中含有NA no yes三个级别的值， 默认把NA包含进截距中，并分别对no yes拟合
lm.fit <- lm(log(PageViews) ~ InEnglish,
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.03122206

#############################################################
#定义相关性
x <- 1:10
y <- x ^ 2

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Twenty-seventh snippet
cor(x, y)
#[1] 0.9745586

# Twenty-eighth snippet
coef(lm(scale(y) ~ scale(x)))
# (Intercept) scale(x)
#-1.386469e-16 9.745586e-01
