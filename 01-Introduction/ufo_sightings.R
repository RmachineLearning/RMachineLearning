# File-Name:       ufo_sightings.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 1.  In this case we will review some of the basic
#                   R functions and coding paradigms we will use throughout this book.
#                   This includes loading, viewing, and cleaning raw data; as well as
#                   some basic visualization.  This specific case we will use data from
#                   reported UFO sightings to investigate what, if any, seasonal trends
#                   exists in the data.
# Data Used:       http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
# Packages Used:   ggplot2, plyr, scales

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# Load libraries and data
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(plyr)       # For data manipulation
library(scales)     # We'll need to fix date formats in plots

# This is a tab-delimited file, so we use 'read.delim' and set the separator as a tab character.
# We also have to alter two defaults; first, we want the strings to not be converted to
# factor types; and, this data has does not have header labels in the first row, so
# we want to keep the first row as data.
#
setwd("/media/zhoutao/软件盘/workspace/R/machine learning/01-Introduction")
setwd("E:/workspace/R/machine learning/01-Introduction")
#在上出目录中，读取目录dataxia下的ufo目录下的tsv文件
#该文件是制表符分隔到，用sep = '\t'分割；
#默认条件下，read.delim将字符读成factor因子，stringsAsFactors = FALSE不将字符读成因子
#na.strings = '' 将空元素设置为NA
ufo <- read.delim(file.path("data", "ufo", "ufo_awesome.tsv"),
                  sep = "\t",
                  stringsAsFactors = FALSE,
                  header = FALSE, 
                  na.strings = "")
# This is a large text file (75MB), so this may take a moment

# Inspect the data frame
summary(ufo)
head(ufo)

# From the data's description file, we will set the column names accordingly using 
# the 'names' function
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")
head(ufo)
# To work with the dates, we will need to convert the YYYYMMDD string to an R Date
# type using the 'strptime' function
#我们要将日期字符串改为as.Date对象
#下面这个代码会报错，因为时间字符串太长，导致日期格式字符串无法匹配
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%y%m%d")


# But, something has gone wrong with the data. For now, we'll just ignore the errata
# by removing those entries that have not parsed correctly.  We know that the date 
# strings are always 8 characters long, and any deviation from this would indicate
# a row to ignore.  We will use the 'ifelse' function to construct a vector of
# Booleans indicating the problem rows
#as.Date日期格式是YYYYMMDD为8个字符，所以我们要找出哪些日期字符串不是8给字符
#不等于8个字符的为FALSE,等于8个字符的为TRUE
good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 |
                    nchar(ufo$DateReported) != 8,
                    FALSE,
                    TRUE)
#一共有61870行观测值
length(good.rows)
#找出，有多少行是字符不等于8的，
#length计算的是TRUE的长度，所以!good.rows将不等于8的FALSE改为TRUE
length(which(!good.rows))      # While 731 rows may seem like a lot, out of over 60K
#因为不等于8个字符的日期只占0.6%，所以直接去掉不要了
#下面也只返回为TRUE的行
ufo <- ufo[good.rows, ]        # it is only about 0.6% of the total number of records.

# Now we can convert the strings to Date objects and work with them properly
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")

# It will be useful to create separate columns for both town and state from the Location 
# column.  To do so we will use the 'strsplit' function to perform the regex.
# Note: not every entry in Location is of the form 'City, State'.  We use the
# 'tryCatch' function to simply return [NA, NA] when this is the case.  Next,
# we remove the leading white-space from both the city and state strings with 'gsub'
#location变量中含有城市和州，因为我们只对美国UFO目击数据变化趋势感兴趣，所以要把
#美国的数据单独跳出来，所以要分词
#定义一个输入为字符串的函数，执行数据清洗工作
get.location <- function(l)
{
  #1 首先用异常处理函数tryCath()包围strsplit()函数
  #当strsplit函数遇到不符合格式的数据会抛出异常(例如该字符串中没有逗号)
  #因此要捕捉(catch)这个异常,不包含逗号的数据，返回一个NA向量来表示这条数据无效
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  
  #2 其次，用正则表达式函数gsub()移除每个字符串开头的一个空格(^后面有一个空格)
  clean.location <- gsub("^ ","",split.location)
  
  #3 许多非美国地名中会返回多个逗号，导致strsplit函数返回向量长度大于2
  #此时依然返回NA向量
  if (length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

# We use 'lapply' to return a list with [City, State] vector as each element
#lapply返回一个list链表,因为每条字符串被分隔成为了两个字符串，所以用链表存储最好
#list是一个“键-值”对形式的数据结构，键由双方括号索引，值在单方括号中
city.state <- lapply(ufo$Location, get.location)
head(city.state)

# We use 'do.call' to collapse the list to an N-by-2 matrix
#do.call是一个list上执行调用的函数，经常与lapply结合使用， 
#传入rbing函数，将city.state链表所有向量(每一个方括号代表一个向量)按行拼接起来，创建一个举止
location.matrix <- do.call(rbind, city.state)

# Add the city and state data to ufo data frame. We can do this using the 'transform'
# function.
#把上面创建的矩阵添加到原始数据中，用transform()
#由于州名缩写不一致，所以在这里用tolower把所有州名字缩写改为小写, toupper大写
ufo <- transform(ufo,
                 USCity = location.matrix[, 1],
                 USState = toupper(location.matrix[, 2]),
                 stringsAsFactors = FALSE)

head(ufo)
################################################################
# Next step, we will strip out non-US incidents
#接下来处理费美国境内的数据
#有些city 和state符合上述数据处理的要求，但是可能ufo目击地在加拿大不在美国
#为了得到美国地区的数据，我们构建美国各州缩写的向量，让USState列向量进行匹配
#把匹配上的保留下来，从而识别出非美国地名

# Insert NA's where there are non-US cities
#names(ufo)
#str(state.abb) 是书中的us.states
#match()函数有两个参数，第一个参数是被匹配的向量，第二参数是用于匹配的向量
#返回值是一个长度与第一参数相同的向量，这个向量中的值时第二个参数中相匹配值的索引，
#如果在第二个参数中没有找到匹配的值，函数默认返回NA
ufo$USState <- state.abb[match(ufo$USState, state.abb)]
head(ufo)
# Finally, we'll use 'subset' to examine only events in the United States and convert 
# states to factors, i.e., a categorical variable.
#因为我们只关心非美国的数据，所以用is.na找到NA的值（是NA返回TRUE, 不是NA返回FALSE），
#!is.na(USState)加！得到美国的数据的行索引
#用subset保留usa的数据
ufo.us <- subset(ufo, !is.na(USState))
head(ufo.us)


###########################################
#聚合并组织数据
# Now, we are ready to do some analysis!  First, take a look at the post-processed data
#因为我们关心目击地点和时间两个维度，所先用summary查看一下每个变量的范围，特别是时间变量
summary(ufo.us)


# The summary functions shows us that the data actually go back a very long time (1440!).  So, 
# we will want to take a quick look at the date to see where the majority of the data exists.
# We can do this by creating a histogram of frequencies for UFO sightings over time
#由于时间夸得很长最古老时间为1400年，这是一个异常值的年份，
#数据在时间上述如何分布的，是否值得用时间序列分析，#我们用直方图看一下
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")

#ggsave()将可视化结果输出到文件里  
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 8)
#也可以用print输出到屏幕上，下面的警告是告诉在默认情形下是如何给数据划分区间的
print(quick.hist)
# First, we notice that there are many very old entries in the data.  For our purposes, we will only look
# at incidents that occurred from 1990 to the most recent
#从图中可以看出1990之后的观测值比较多，前面的观测值看成是异常值，不使用
#as.Date类型的数据是可以进行比较的
ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))
nrow(ufo.us)
# Let's look at the histogram now

new.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(breaks = "50 years")

ggsave(plot = new.hist,
       filename = file.path("images", "new_hist.pdf"),
       height = 6,
       width = 8)
print(new.hist)
# Now that we have the data we want, let's look at some aggregations.  We will use
# the 'ddply' funtion in the plyr package. But first, we create a column of just
# the Year-Month of each incident.
#我们要探究的问题是美国境内目击到UFO的记录是否有周期性
#DateOccurred的数据是精确到天的，我们按照年-月聚合，来查看周期性
#首先我们用strftime将日期对象转化成YYYY-MM格式
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format = "%Y-%m")

# This will return the number of sightings of UFO by Year-Month and state for the whole time-series
#ddply{plyr}
#分组:先按州分，然后按年-月分
#聚合：对每个分组分别计算行数(nrow)，然后组合到一起
sightings.counts <- ddply(ufo.us, .(USState,YearMonth), nrow)
#head(sightings.counts)

# As we might expect, there are several Year-Month and state combinations for which there are no 
# UFO sightings.  We need to count these as zero so we can go back and fill those in.
# First, we will create a new vector that has all of the Year-Month dates in it that span the 
# range of our time-series (1990-2010)
#从head()中可以看出有些月份如2、4月没有目击记录，
#但是数据集中的这些位置也没有包含无ufo目击事件发生的记录，因此我们要记它们为0次
#首先我们需要覆盖整个数据集的“年-月”向量，用这个向量查看哪些“年-月”已经存在于数据集中，
#如果不存在就补上，并赋值为0
#用sep.Date创建时间序列
date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by = "month")
#转化为“年-月”时间格式
date.strings <- strftime(date.range, "%Y-%m")

# To fill in the missing dates from the 'sightings.counts' data frame we will need to create a separate data
# frame with a column of states and Year-Months.
#新建一个包含所有“年-月”和州的数据框
states.dates <- lapply(state.abb, function(s) cbind(s, date.strings))
#用do.call将list链表转化为矩阵，然后用data.frame 转化为数据框
states.dates <- data.frame(do.call(rbind, states.dates),
                           stringsAsFactors = FALSE)

# We use 'merge' to take the counts we have and merge them with the missing dates.  Note, we have to specify
# the columns from each data frame we are using to do the merge, and set 'all' to TRUE, which will fill in 
# this missing dates from the original data frame with NA.
#by.x和by.y分别对应要合并的列名
#all =TRUE告诉函数，将没有匹配的数据包含进来，并填充为NA，下列v1列中为NA的记录即为没有UFO目击数据
all.sightings <- merge(states.dates,
                       sightings.counts,
                       by.x = c("s", "date.strings"),
                       by.y = c("USState", "YearMonth"),
                       all = TRUE)
head(all.sightings)
# Now we just need to clean up the merged data frame a bit
# Set the column names to something meaningful
names(all.sightings) <- c("State", "YearMonth", "Sightings")

# Covert the NAs to 0's, what we really wanted
#将NA部分填充 为0次
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0

# Reset the character Year-Month to a Date objects
#每个州对应的天数是一样的，所以用rep函数，然后转化为as.Date对象
#日期最好用Date对象保存，比字符串要好
all.sightings$YearMonth <- as.Date(rep(date.range, length(state.abb)))

# Capitalize the State abbreviation and set as factor
#地名最好用分类变量表示即factor因子，而不是以字符串形式保存
all.sightings$State <- as.factor(all.sightings$State)



#############################################################################
##分析数据
# There are lots of ways we could test the seasonality of of these sightings, but one basic method is to 
# inspect the trends visually.  We now construct a plot that will show these trends for all 50 U.S. states
# over the time-series.


# First we have to create a ggplot2 object and then create a geom layer, which in this case is a line.
# Additional points of note:
# (1) facet_wrap() will create separate plots for each state on a 10x5 grid.
# (2) theme_bw() changes the default ggplot2 style from grey to white (personal preference).
# (3) scale_color_manual() sets the line color to dark blue.
# (4) scale_x_date() scales the x-axis as a date, with major lines every 5 years.
# (5) xlab() and ylab() set axis labels.
# (6) opts() sets a title for the plot
#第一步，用数据框作为第一个参数来创建ggplot对象
#创建一个图层，x轴为x = YearMonth, y轴为y = Sightings
state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  #为了表现各州的周期性变化，给每一个州绘制一副曲线图，可以方便我们观察每个州
  #UFO目击次数随着时间的变化的峰值 低谷 波动区， 线条颜色dakblue深蓝色
  geom_line(aes(color = "darkblue")) +
  #创建分块绘制图形，并指明图形面板的构造使用State变量，它是一个factor类型分类变量
  #并定义网格的行数和列数
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  #默认的是绘制主题是灰色背景，深灰色网格线，
  #为了更容易看清楚数据之间不同之处，这里添加theme_bw()层，该图层为白色背景，黑色网格线
  theme_bw() + 
  #用来指明字符串"darkblue"相当于网页安全色"darkblue"，
  #ggplot2倾向用颜色区分分类变量，即用factor类型指明颜色
  #我们明确将颜色定义成一个字符串类型，因此还要用scale_color_manual函数定义这个字符串的值
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  #用来指明可视化结果中主要网格线，因为这份数据的跨度为20年，所以我们间隔设置为5年
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  #用ggtitle赋一个主题
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

# Save the plot as a PDF
#用ggsave将结果渲染成图像
ggsave(plot = state.plot,
       filename = file.path("images", "ufo_sightings.pdf"),
       width = 14,
       height = 8.5)
print(state.plot)

# Create a new graph where the number of signtings is normailzed by the state population
state.pop <- read.csv(file.path('data/census.csv'), stringsAsFactors=FALSE)

state.pop$abbs <- sapply(state.pop$State, function(x) state.abb[grep(paste('^', x, sep=''), state.name)])
all.sightings$Sightings.Norm <- sapply(1:nrow(all.sightings), 
    function(i) all.sightings$Sightings[i] / state.pop$X2000[which(state.pop$abbs== all.sightings$State[i])])
    
    
state.plot.norm <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings.Norm)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
  
  
# Save the plot as a PDF
ggsave(plot = state.plot.norm,
     filename = file.path("images", "ufo_sightings_norm.pdf"),
     width = 14,
     height = 8.5)
