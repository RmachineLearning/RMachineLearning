# File-Name:       priority_inbox.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 4.  In this case study we will attempt to write a "priority
#                   inbox" algorithm for ranking email by some measures of importance.  We will
#                   define these measures based on a set of email features, which moves beyond
#                   the simple work counts used in Chapter 3.
# Data Used:       Email messages contained in ../../03-Classification/code/data/
#                   source: http://spamassassin.apache.org/publiccorpus/
# Packages Used:   tm, ggplot2, plyr

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!
#排序：智能收件箱                       
# Load libraries
setwd("E:/workspace/R/machine learning/04-Ranking")
setwd("/media/zhoutao/软件盘/workspace/R/machine learning/04-Ranking")
library('tm')
library('ggplot2')
library('plyr')

# Set the global paths
#从上述路径中退回（..）到03-Classification目录中，然后进入data目录中
data.path <- file.path("..", "03-Classification", "data")
#在这个路劲中找到easy_ham目录
easyham.path <- file.path(data.path, "easy_ham")

# We define a set of function that will extract the data
# for the feature set we have defined to rank email
# impportance.  This includes the following: message
# body, message source, message subject, and date the
# message was sent.

# Simply returns the full text of a given email message
#打开该路径中的文件，将整封邮件作为一个字符串向量返回
msg.full <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

# Retuns the email address of the sender for a given
# email message
#抽取特征1：抽取发件人的地址
#grepl与grep函数一样，用于匹配正则表达式模式串，不同的是后面的l代表（logical）
#表示它返回的不是向量索引号，而是返回与msg.vec长度一样的向量，这个向量中的元素用于
#标识字符串向量每个元素是否匹配上模式串
#用grepl先匹配到有（from：）的行
#strsplit将字一个字符串拆分成一个列表，用方括号为拆分模式创建一个字符集
#字符集中包含（冒号 尖括号 空格），那么第一个字符串为发件人地址
#因为文本模式存在变化，取[[1]] 有可能取不到，而是得到空格，所以我们要忽略空格
#然后查找包含@的元素并返回它们
get.from <- function(msg.vec)
{
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from  != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

# Retuns the subject string for a given email message
#抽取特征2：邮件主题
#由于不是所有邮件都有主题，那么就要考察subj长度是否大于0
#大于0就把这一行按照模式拆分，并返回拆分得到的第二个元素
#否则返回空字符
get.subject <- function(msg.vec)
{
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0)
  {
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else
  {
    return("")
  }
}

# Similar to the function from Chapter 3, this returns
# only the message body for a given email.
#抽取特征3：邮件正文
get.msg <- function(msg.vec)
{
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

# Retuns the date a given email message was received
#抽取特征4：收件箱邮件时间（处理时间相当困难）
#第一：不同编程语言对时间的设计理念不一样，R将日期字符转化为POSIX日期对象，然后对日期排序
#第二：邮件接收的日期和时间形式存在很大差异
#1 邮件时间中总是包含Date：，但是很多行匹配Date：
#2 日期时间不是同一的表达方式，但是所有邮件中都有一附加的格林尼治时间（GMT）偏移量
#在这里我么要剔除GMT抽取日期和时间

#通过观察发现只有一行所包含Date：在字符串首部，故用^Date完成
#只有在首部发现模式串时才返回TRUE
#因为时间总是在正文前面，如果正文中有一行首部也出现了Date：，为了保证不出错
#将第一个匹配上的元素作为返回对象
#日期和时间字符串得到后，要将他们转化为POSIX对象，
#首先要把日期和时间分离出来，用字符串拆分，从而标记多余信息
#分离出时间后，用gsub将首部或者尾部的空白字符替换掉
#标准的日期时间是25个字符，strtrim()将其后面的字符被裁减掉
get.date <- function(msg.vec)
{
  date.grep <- grepl("^Date: ", msg.vec)
  date.grep <- which(date.grep == TRUE)
  date <- msg.vec[date.grep[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

# This function ties all of the above helper functions together.
# It returns a vector of data containing the feature set
# used to categorize data as priority or normal HAM
#将邮件四种特征转化为结构化的数据方块
parse.email <- function(path)
{
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

# In this case we are not interested in classifiying SPAM or HAM, so we will take
# it as given that is is being performed.  As such, we will use the EASY HAM email
# to train and test our ranker.
#得到目录中的文件名
easyham.docs <- dir(easyham.path)
#去掉cmds文件
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
#对每一封邮件，抽取四种特征
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(file.path(easyham.path, p)))

# Convert raw data from list to data frame
#将列表转化为数据框结构，将列表中的名称与对应的邮件特征按行排列
#也就是每一封邮件的特征排一行，
ehparse.matrix <- do.call(rbind, easyham.parse)
#将字符串改为因子
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")
#head(allparse.df)
# Convert date strings to POSIX for comparison. Because the emails data
# contain slightly different date format pattners we have to account for
# this by passining them as required partmeters of the function. 
#抽取日期所做的第一步只是简单的文本分类，现在要将文本转化为POSIX对象，以便逻辑比较
#由于日期时间格式不同，通过strptime()传入不同时间/日期格式来转化为POSIX对象
#不匹配的格式将返回NA, 因此，可以将第二个匹配的代替第一不匹配的NA
#这样就转化为相同格式的日期时间
#为了克服系统字符编码问题，增加了lct作为转换方法
date.converter <- function(dates, pattern1, pattern2)
{
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  
  Sys.setlocale("LC_TIME", lct)
  
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"


allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

head(allparse.df$Date)

# Convert emails and subjects to lower-case
#将主题和发件人转化为小写，为的是格式尽量统一
allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

# Order the messages chronologically
#按照时间对数据排序
#按照时间升序数据索引好排列
priority.df <- allparse.df[with(allparse.df, order(Date)), ]
#head(priority.df)
# We will use the first half of the priority.df to train our priority in-box algorithm.
# Later, we will use the second half to test.
#前半部分作为排序算法到训练集；后半部分作为测试集
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]
dim(priority.train)
# The first step is to create rank weightings for all of the features.
# We begin with the simpliest: who the email is from.
# Calculate the frequency of correspondence with all emailers in the training set
#from.weight <- ddply(priority.train, .(From.EMail), summarise, Freq = length(Subject))
#length(priority.train$From.EMail)

#####################################################
#用于排序的权重计算策略
#计算发件人所发邮件到频数，并排序
library('reshape2')
from.weight <- melt(with(priority.train, table(From.EMail)), 
                    value.name="Freq")

from.weight <- from.weight[with(from.weight, order(Freq)), ]

# We take a subset of the from.weight data frame to show our most frequent 
# correspondents.
#找出大于6封邮件到发件人
from.ex <- subset(from.weight, Freq > 6)
head(from.ex)
from.scales <- ggplot(from.ex) +
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,
                xmax = 1:nrow(from.ex) + 0.5,
                ymin = 0,
                ymax = Freq,
                fill = "lightgrey",
                color = "darkblue")) +
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +
  coord_flip() +
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  #不要背景中的网格线
  theme_bw() +
  theme(axis.text.y = element_text(size = 5, hjust = 1))

ggsave(plot = from.scales,
       filename = file.path("images", "0011_from_scales.pdf"),
       height = 4.8,
       width = 7)

# Log weight scheme, very simple but effective
#因为有些邮件太多，而有些太少，这样给权重就不好设定，为了避免特征数值不那么极端
#要进行对数变化
#log为ln的对数变换
#log10的对数变换，使上图到数据更加平稳
#因为1的对数是0，所以为了避免权重出现0到情况（有些频数为1），就在频数上再加1
from.weight <- transform(from.weight,
                         Weight = log(Freq + 1),
                         log10Weight = log10(Freq + 1))

from.rescaled <- ggplot(from.weight, aes(x = 1:nrow(from.weight))) +
  geom_line(aes(y = Weight, linetype = "ln")) +
  geom_line(aes(y = log10Weight, linetype = "log10")) +
  geom_line(aes(y = Freq, linetype = "Absolute")) +
  scale_linetype_manual(values = c("ln" = 1,
                                   "log10" = 2,
                                   "Absolute" = 3),
                        name = "Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

ggsave(plot = from.rescaled,
       filename = file.path("images", "0012_from_rescaled.pdf"),
       height = 4.8,
       width = 7)

#########################################################
# To calculate the rank priority of an email we should calculate some probability that 
# the user will respond to it.  In our case, we only have one-way communication data.
# In this case, we can calculate a weighting based on words in threads that have a lot
# of activity.

# This function is used to find threads within the data set.  The obvious approach
# here is to use the 're:' cue from the subject line to identify message threads.
#邮件线程活跃度的权重计算
#邮件第二个特征是线程行为
#主题以re：开始，那么我们就知道这是某个线程到一部分，
#当发现这样的邮件时，我们再找以下这个线程里的其他邮件，并测量其活跃度
#对含有re：到主题进行拆分，第一个元素为空字符串(含有re的主题)的字符向量来找到各个线程
find.threads <- function(email.df)
{#email.df = priority.train
  response.threads <- strsplit(email.df$Subject, "re: ")
  is.thread <- sapply(response.threads,
                      function(subj) ifelse(subj[1] == "", TRUE, FALSE))
  threads <- response.threads[is.thread]
  senders <- email.df$From.EMail[is.thread]
  threads <- sapply(threads,
                    function(t) paste(t[2:length(t)], collapse = "re: "))
  return(cbind(senders,threads))
}

threads.matrix <- find.threads(priority.train)
head(threads.matrix)
# Using the matrix of threads generated by the find.threads function this function
# creates a data from of the sender's email, the frequency of emails from that
# sender, and a log-weight for that sender based on the freqeuncy of corresponence.
#根据线程中最活跃的发件人赋予权重，生成第二个基于数量的权重(频数加1后取对数)
email.thread <- function(threads.matrix)
{
  senders <- threads.matrix[, 1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq),
                          senders.freq,
                          log(senders.freq + 1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors=FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.EMail", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(threads.matrix)
head(senders.df)
# As an additional weight, we can enhance our notion of a thread's importance
# by measuring the time between responses for a given email.  This function
# takes a given thread and the email.df data frame to generate a weighting 
# based on this activity level.  This function returns a vector of thread
# activity, the time span of a thread, and its log-weight.
#下面给已知线程增加权重
#如果这些线程已知，用户会觉得这些活跃线程更重要
#下面要衡量一个线程到活跃度
#利用线程主题和训练数据作为参数，收集线程向量中所有邮件日期和时间戳
#计算出训练数据中这个线程接受了多少封邮件（就是计算thread.times长度）
#最后计算活跃度，需要计算这个线程在训练数据中存在了多长时间difftime()计算时间差，
#secs表示时间单位为秒
#有些线程只有一封邮件，那么它所有返回对象为NA，然后我们会将这些只有一封邮件的线程从
#活跃度权重数据中剔除

#给那些大于1的线程赋予权重
#首先计算每个线程在单位时间内邮件到达率（每秒收到几封邮件）
#由于 权重(trans.weight)会变成小数，在log10下会变成负权重，所以通过仿设变换（即加10移动所有的点）
thread.counts <- function(thread, email.df)
{
  # Need to check that we are not looking at the original message in a thread, 
  # so we check the subjects against the 're:' cue.
  thread.times <- email.df$Date[which(email.df$Subject == thread |
                                      email.df$Subject == paste("re:", thread))]
  freq <- length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(difftime(max.time, min.time, units = "secs"))
  if(freq < 2)
  {
    return(c(NA, NA, NA))
  }
  else
  {
    trans.weight <- freq / time.span
    log.trans.weight <- 10 + log(trans.weight, base = 10)
    return(c(freq, time.span, log.trans.weight))
  }
}

# This function uses the threads.counts function to generate a weights
# for all email threads.
#对所有邮件构建线程大于q1的权重
get.threads <- function(threads.matrix, email.df)
{
  threads <- unique(threads.matrix[, 2])
  thread.counts <- lapply(threads,
                          function(t) thread.counts(t, email.df))
  thread.matrix <- do.call(rbind, thread.counts)
  return(cbind(threads, thread.matrix))
}

# Now, we put all of these function to work to generate a training set
# based on our thread features.
thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors = FALSE)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE)

# Similar to what we did in Chapter 3, we create a simple function to return a 
# vector of word counts.  This time, however, we keep the TDM as a free
# parameter of the function.
#将邮件正文内容构建成一个矩阵
term.counts <- function(term.vec, control)
{
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
  return(rowSums(as.matrix(vec.tdm)))
}
head(thread.weights)
#出现在活跃线程中的高频词汇比在不活跃度线程的词汇要重要
thread.terms <- term.counts(thread.weights$Thread,
                            control = list(stopwords = TRUE))
thread.terms <- names(thread.terms)

term.weights <- sapply(thread.terms,
                       function(t) mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed = TRUE)]))

term.weights <- data.frame(list(Term = names(term.weights),
                                Weight = term.weights),
                           stringsAsFactors = FALSE,
                           row.names = 1:length(term.weights))

# Finally, create weighting based on frequency of terms in email. 
# Will be similar to SPAM detection, but in this case weighting
# high words that are particularly HAMMMY.

msg.terms <- term.counts(priority.train$Message,
                         control = list(stopwords = TRUE,
                         removePunctuation = TRUE,
                         removeNumbers = TRUE))

msg.weights <- data.frame(list(Term = names(msg.terms),
                               Weight = log(msg.terms, base = 10)),
                          stringsAsFactors = FALSE,
                          row.names = 1:length(msg.terms))

# Remove words that have a zero weight
msg.weights <- subset(msg.weights, Weight > 0)

# This function uses our pre-calculated weight data frames to look up
# the appropriate weightt for a given search.term.  We use the 'term'
# parameter to dertermine if we are looking up a word in the weight.df
# for it message body weighting, or for its subject line weighting.
#训练和排序测试算法
#为了给训练数据每一封邮件产生一个优先等级，必须将前季节生成的所有权重相乘

#输入三个参数：要查找到词项（一个字符串）；要查找到对象（权重数据框）；词项（term）的一个布尔值
#最后一个参数告诉应用程序要查找的数据框是词项数据框还是线程数据框
#如果待查词项length(search.term)=0，表示无效 词项，返回1这样乘以1不影响计算
#match会搜索向量中所有没有匹配上searh.term的元素返回NA值，抽出那些匹配的元素，即返回非NA的值元素的权重值
#term.match为NA的值会导致match.weights的长度为0，因此要进一步检查，将此类值返回为1
#如果匹配了若干权重值，那么这些权重值的均值作为返回值
get.weights <- function(search.term, weight.df, term = TRUE)
{
  if(length(search.term) > 0)
  {
    if(term)
    {
      term.match <- match(names(search.term), weight.df$Term)
    }
    else
    {
      term.match <- match(search.term, weight.df$Thread)
    }
    
    match.weights <- weight.df$Weight[which(!is.na(term.match))]
    
    if(length(match.weights) < 1)
    {
      return(1)
    }
    else
    {
      return(mean(match.weights))
    }
    
  }
  else
  {
    return(1)
  }
  
}

# Our final step is to write a function that will assign a weight to each message based
# on all of our, we create a function that will assign a weight to each message based on
# the mean weighting across our entire feature set.
#从数据集中每一封邮件抽取特征赋予权重

#首先调用pars.email()抽取我们感兴趣的四个特征，
#然后用一系列ifelse语句判定抽取到特征是否出现在某个用于排序的权重数据框中
#如果ifelse没有匹配到任何权重，则返回1，这个方法与get.Weights()函数一样

#对于线程和词项要做一些文本解析工作
#对于线程：首先检查待排序邮件是否属于某个线程，如果属于某个线程，那么查询其活跃度排序
#否则赋权重为1

rank.message <- function(path)
{
  msg <- parse.email(path)
  
  from <- ifelse(length(which(from.weight$From.EMail == msg[2])) > 0,
                 from.weight$Weight[which(from.weight$From.EMail == msg[2])],
                 1)
  
  # Second is based on senders in threads, and threads themselves
  thread.from <- ifelse(length(which(senders.df$From.EMail == msg[2])) > 0,
                        senders.df$Weight[which(senders.df$From.EMail == msg[2])],
                        1)
  
  subj <- strsplit(tolower(msg[3]), "re: ")
  is.thread <- ifelse(subj[[1]][1] == "", TRUE, FALSE)
  if(is.thread)
  {
    activity <- get.weights(subj[[1]][2], thread.weights, term = FALSE)
  }
  else
  {
    activity <- 1
  }
  
  # Weight based on terms in threads
  #对于词项：用term.counts()函数从邮件特征中获取相关词，并获取相应权重，   
  thread.terms <- term.counts(msg[3], control = list(stopwords = TRUE))
  thread.terms.weights <- get.weights(thread.terms, term.weights)
  
  # Weight based terms in all messages
  msg.terms <- term.counts(msg[4],
                           control = list(stopwords = TRUE,
                           removePunctuation = TRUE,
                           removeNumbers = TRUE))
  msg.weights <- get.weights(msg.terms, msg.weights)
  
  # Calculate rank by interacting all weights
  #把所有权重值传给prod()产生rank（优先级排序值）
  rank <- prod(from,
               thread.from,
               activity, 
               thread.terms.weights,
               msg.weights)
  #rank.message()函数返回一个含有邮件日期/时间，发件人地址，主题，和优先级排序值到向量
  return(c(msg[1], msg[2], msg[3], rank))
}

# Find splits again
#准备启用排序算法，
#首先将数据安时间拆分成两部分 
#训练集train.paths; 测试集test.paths
train.paths <- priority.df$Path[1:(round(nrow(priority.df) / 2))]
test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):nrow(priority.df)]

# Now, create a full-featured training set.
#在train.paths上应用rank.message()函数，产生一个列表，其中包含每一封邮件的特征和优先级排序值
#然后把列表向量转化为数据框结构
# suppressWarnings()用来关闭警告
train.ranks <- suppressWarnings(lapply(train.paths, rank.message))
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.paths, train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)
names(train.ranks.df) <- c("Message", "Date", "From", "Subj", "Rank", "Type")
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

# Set the priority threshold to the median of all ranks weights
#计算优先邮件的阀值，
#选择中位数作为阀值到原则：
# 1 如果排序算法足够好，排序结果应该是一个平滑的分布，大多数邮件的优先级
#排序较低，少数邮件优先级排序较高，那么这些就是排序分布中合格的部分，
#直观表现就是，挑选的邮件优先级排序值应该是大于一封典型邮件的排序值

priority.threshold <- median(train.ranks.df$Rank)

# Visualize the results to locate threshold
threshold.plot <- ggplot(train.ranks.df, aes(x = Rank)) +
  stat_density(aes(fill="darkred")) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_fill_manual(values = c("darkred" = "darkred"), guide = "none") +
  theme_bw()
ggsave(plot = threshold.plot,
       filename = file.path("images", "01_threshold_plot.pdf"),
       height = 4.7,
       width = 7)

# Classify as priority, or not
# 2 我们知道测试集中有些邮件的特征值在训练数据中时完全没有出现过的，新邮件的不断增加，
#在没办法更新排序算法时，我们倾向于包容性而非排他性的优先级原则，如果不这样做会丢失只匹配
#了部分特征的邮件，所有给train.ranks.df增加列priority(二值向量)
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)

# Now, test our ranker by performing the exact same procedure on the test data
test.ranks <- suppressWarnings(lapply(test.paths,rank.message))
test.ranks.matrix <- do.call(rbind, test.ranks)
test.ranks.matrix <- cbind(test.paths, test.ranks.matrix, "TESTING")
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors = FALSE)
names(test.ranks.df) <- c("Message","Date","From","Subj","Rank","Type")
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)
test.ranks.df$Priority <- ifelse(test.ranks.df$Rank >= priority.threshold, 1, 0)

# Finally, we combine the data sets.
final.df <- rbind(train.ranks.df, test.ranks.df)
final.df$Date <- date.converter(final.df$Date, pattern1, pattern2)
final.df <- final.df[rev(with(final.df, order(Date))), ]
head(final.df)
# Save final data set and plot results.
write.csv(final.df, file.path("data", "final_df.csv"), row.names = FALSE)

testing.plot <- ggplot(subset(final.df, Type == "TRAINING"), aes(x = Rank)) +
  stat_density(aes(fill = Type, alpha = 0.65)) +
  stat_density(data = subset(final.df, Type == "TESTING"),
               aes(fill = Type, alpha = 0.65)) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_alpha(guide = "none") +
  scale_fill_manual(values = c("TRAINING" = "darkred", "TESTING" = "darkblue")) +
  theme_bw()
ggsave(plot = testing.plot,
       filename = file.path("images", "02_testing_plot.pdf"),
       height = 4.7,
       width = 7)
