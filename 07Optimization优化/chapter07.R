# File-Name:       chapter07.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/01_heights_weights_genders.csv, data/lexical_database.Rdata
# Packages Used:   n/a

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!
# 优化;密码破解
#构建一个简单密码破译系统，把它解密一串密文当做一个优化问题
setwd("E:/workspace/R/machine learning/07-Optimization")
# First code snippet
height.to.weight <- function(height, a, b)
{
  return(a + b * height)
}

# Second code snippet
heights.weights <- read.csv(file.path('data', '01_heights_weights_genders.csv'))

coef(lm(Weight ~ Height, data = heights.weights))
#(Intercept) Height
#-350.737192 7.717288

# 要优化a 和b ,求最小的均方误
squared.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2))
}

#a = [-1 1] b = [-1 1]之间寻找最优值
#下面就是网格搜索方法求解，但是问题在于不能确定搜索步长
for (a in seq(-1, 1, by = 1))
{
  for (b in seq(-1, 1, by = 1))
  {
    print(squared.error(heights.weights, a, b))
  }
}

# optim()封装了很多优化方法
optim(c(0, 0),
      function (x)
      {
        squared.error(heights.weights, x[1], x[2])
      })
#$par
#[1] -350.786736    7.718158
#
#$value 返回最优点时，平方误差值
#[1] 1492936
#
#$counts 告诉执行的函数以及梯度多少次
#function gradient 
#     111       NA 
#
#$convergence 是否收敛到最优点
#[1] 0
#
#$message 保存了可能有用的信息
#NULL

#为了找到最优的a，把均方误看成是关于a的函数
a.error <- function(a)
{
  return(squared.error(heights.weights, a, 0))
}

# 使用curve()函数将不同的a与均方误的曲线
curve(sapply(x, function (a) {a.error(a)}), from = -1000, to = 1000)

# Eighth code snippet
b.error <- function(b)
{
  return(squared.error(heights.weights, 0, b))
}

curve(sapply(x, function (b) {b.error(b)}), from = -1000, to = 1000)

########################################################
#岭回归 
#岭回归 与OLS的区别是：岭回归包含了正则化，把回归系数本身当做误差以部分，促使回归系数变小
#lambda参数用来权衡我们是希望有较小的平方误差
#还是希望有较小的a和b避免过度拟合
#误差的平方均方误
ridge.error <- function(heights.weights, a, b, lambda)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2) + lambda * (a ^ 2 + b ^ 2))
}

# 假定最优的lambda是1
lambda <- 1

optim(c(0, 0),
      function (x)
      {
        ridge.error(heights.weights, x[1], x[2], lambda)
      })

#$par
#[1] -340.434108    7.562524
#
#$value
#[1] 1612443
#
#$counts
#function gradient 
#     115       NA 
#
#$convergence
#[1] 0
#
#$message
#NULL

# a改变时，岭回归均方误
a.ridge.error <- function(a, lambda)
{
  return(ridge.error(heights.weights, a, 0, lambda))
}
curve(sapply(x, function (a) {a.ridge.error(a, lambda)}), from = -1000, to = 1000)

b.ridge.error <- function(b, lambda)
{
  return(ridge.error(heights.weights, 0, b, lambda))
}
curve(sapply(x, function (b) {b.ridge.error(b, lambda)}), from = -1000, to = 1000)

# 绝对误差正则化
absolute.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(abs(errors)))
}

# Thirteenth code snippet
a.absolute.error <- function(a)
{
  return(absolute.error(heights.weights, a, 0))
}
#绝对误差曲线比平方误差或者岭误差曲线要锋利的多，
#以致于optim函数无法通过一个单独的点来获知他该向哪个方向前进，
#进而无法达到全局最优，尽管我们可以看到确实存在全局最优点

curve(sapply(x, function (a) {a.absolute.error(a)}), from = -1000, to = 1000)


#############################################################################
#密码破译优化问题

#首先实现凯撒密码
english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                     'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                     'w', 'x', 'y', 'z')
#空列表
caesar.cipher <- list()

inverse.caesar.cipher <- list()
#让每个字母变成其后面1位字母，即a 变成b; b变成c  %%取余数
#caesar.cipher包含字母变换的
for (index in 1:length(english.letters))
{
  caesar.cipher[[english.letters[index]]] <- english.letters[index %% 26 + 1]
  inverse.caesar.cipher[[english.letters[index %% 26 + 1]]] <- english.letters[index]
}

print(caesar.cipher)

#写一个明文加密成密文的函数
apply.cipher.to.string <- function(string, cipher)
{
  output <- ''

  for (i in 1:nchar(string))
  {
  output <- paste(output, cipher[[substr(string, i, i)]], sep = '')
  }
  
  return(output)
}


apply.cipher.to.text <- function(text, cipher)
{
  output <- c()
  
  for (string in text)
  {
    output <- c(output, apply.cipher.to.string(string, cipher))
  }
  
  return(output)
}

apply.cipher.to.text(c('sample', 'text'), caesar.cipher)

##############################################
#解密的工具 
generate.random.cipher <- function()
{
  cipher <- list()
  
  inputs <- english.letters
  
  outputs <- english.letters[sample(1:length(english.letters), length(english.letters))]
  
  for (index in 1:length(english.letters))
  {
    cipher[[inputs[index]]] <- outputs[index]
  }
  
  return(cipher)
}

modify.cipher <- function(cipher, input, output)
{
  new.cipher <- cipher
  
  new.cipher[[input]] <- output
  
  old.output <- cipher[[input]]
  
  collateral.input <- names(which(sapply(names(cipher),
                                         function (key) {cipher[[key]]}) == output))
  
  new.cipher[[collateral.input]] <- old.output
  
  return(new.cipher)
}

propose.modified.cipher <- function(cipher)
{
  input <- sample(names(cipher), 1)
  
  output <- sample(english.letters, 1)
  
  return(modify.cipher(cipher, input, output))
}

# Seventeenth code snippet
load(file.path('data', 'lexical_database.Rdata'))

# Eighteength code snippet
lexical.database[['a']]
lexical.database[['the']]
lexical.database[['he']]
lexical.database[['she']]
lexical.database[['data']]

# Nineteenth code snippet
one.gram.probability <- function(one.gram, lexical.database = list())
{
  lexical.probability <- lexical.database[[one.gram]]
  
  if (is.null(lexical.probability) || is.na(lexical.probability))
  {
  return(.Machine$double.eps)
  }
  else
  {
  return(lexical.probability)
  }
}

# Twentieth code snippet
log.probability.of.text <- function(text, cipher, lexical.database = list())
{
  log.probability <- 0.0
  
  for (string in text)
  {
    decrypted.string <- apply.cipher.to.string(string, cipher)
    log.probability <- log.probability +
    log(one.gram.probability(decrypted.string, lexical.database))
  }
  
  return(log.probability)
}

# Twenty-first code snippet
metropolis.step <- function(text, cipher, lexical.database = list())
{
  proposed.cipher <- propose.modified.cipher(cipher)
  
  lp1 <- log.probability.of.text(text, cipher, lexical.database)
  lp2 <- log.probability.of.text(text, proposed.cipher, lexical.database)
  
  if (lp2 > lp1)
  {
    return(proposed.cipher)
  }
  else
  {
    a <- exp(lp2 - lp1)
    x <- runif(1)
    
    if (x < a)
    {
      return(proposed.cipher)
    }
    else
    {
      return(cipher)
    }
  }
}

# Twenty-second code snippet
decrypted.text <- c('here', 'is', 'some', 'sample', 'text')

# Twenty-third code snippet
encrypted.text <- apply.cipher.to.text(decrypted.text, caesar.cipher)

# Twenty-fourth code snippet
set.seed(1)

cipher <- generate.random.cipher()

results <- data.frame()

number.of.iterations <- 50000

for (iteration in 1:number.of.iterations)
{
  log.probability <- log.probability.of.text(encrypted.text,
                                             cipher,
                                             lexical.database)
  
  current.decrypted.text <- paste(apply.cipher.to.text(encrypted.text,
                                                       cipher),
                                  collapse = ' ')
  
  correct.text <- as.numeric(current.decrypted.text == paste(decrypted.text,
                                                             collapse = ' '))

  results <- rbind(results,
                   data.frame(Iteration = iteration,
                              LogProbability = log.probability,
                              CurrentDecryptedText = current.decrypted.text,
                              CorrectText = correct.text))
  
  cipher <- metropolis.step(encrypted.text, cipher, lexical.database)
}

write.table(results,
            file = file.path('data/results.tsv'),
            row.names = FALSE,
            sep = '\t')
