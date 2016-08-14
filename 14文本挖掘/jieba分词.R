#分词引擎介绍

#worker()的介绍

#在加载分词引擎时，可以自定义词库路径，同时可以启动不同的引擎：
#最大概率法（MPSegment），负责根据Trie树构建有向无环图和进行动态规划算法，是分词算法的核心。
#隐式马尔科夫模型（HMMSegment）是根据基于人民日报等语料库构建的HMM模型来进行分词，主要算法思路是根据(B,E,M,S)四个状态来代表每个字的隐藏状态。 HMM模型由dict/hmm_model.utf8提供。分词算法即viterbi算法。
#混合模型（MixSegment）是四个分词引擎里面分词效果较好的类，结它合使用最大概率法和隐式马尔科夫模型。
#索引模型（QuerySegment）先使用混合模型进行切词，再对于切出来的较长的词，枚举句子中所有可能成词的情况，找出词库里存在。

#worker() 用于新建分词引擎，可以同时新建多个分词引擎。
#引擎的类型有： mix（混合模型）, mp（最大概率模型）, hmm（HMM模型）, query（索引模型）, 
#tag（标记模型）, simhash（Simhash 模型）和 keywords（关键词模型），共7种。

参数说明

type 引擎类型

引擎的类型有： mix（混合模型）, mp（最大概率模型）, hmm（HMM模型）, query（索引模型）, tag（标记模型）, simhash（Simhash 模型）和 keywords（关键词模型），共7种。

dict 系统词典

优先载入的词典，包括词、词频、词性标记三列。可以输入自定义路径。

hmm HMM模型路径

HMM模型路径

user 用户词典

用户词典，包括词、词性标记两列。用户词典中的所有词的词频均为系统词典中的最大词频。可以输入自定义路径。

idf IDF词典

IDF 词典，关键词提取使用。

stop_word 关键词用停止词库

关键词提取使用的停止词库。

write 写入文件

是否将文件分词结果写入文件，默认为否。只在输入内容为文件路径时，本参数才会被使用。本参数只对分词和词性标注有效。

qmax 最大索引长度

索引模型中，最大可能成词的字符数。

topn 关键词数

提取的关键词数。

encoding 输入文件编码

输入文件的编码，默认为UTF-8。

detect 检测编码

是否检查输入文件的编码，默认检查。

symbol 保留符号

是否保留符号，默认不保留符号。

lines 读取行数

每次读取文件的最大行数，用于控制读取文件的长度。对于大文件，实现分次读取。

output 输出路径

指定输出路径，一个字符串路径。只在输入内容为文件路径时，本参数才会被使用。

bylines 按行输出

文件结果是否按行输出，如果是，则将读入的文件或字符串向量按行逐个进行分词操作。


#中文分词
library(jiebaR)

######分词

#jiebaR提供了四种分词模式，可以通过worker()来初始化分词引擎，使用segment()进行分词。
###  接受默认参数，建立分词引擎 
###########################################################

mixseg = worker()
### 输出worker的设置
mixseg 
#可以通过R语言常用的 $符号重设一些worker的参数设置，如 WorkerName$symbol = T，
#在输出中保留标点符号。一些参数在初始化的时候已经确定，无法修改， 
#可以通过WorkerName$PrivateVarible来获得这些信息。
mixseg$encoding

mixseg$detect = F
##  相当于：
##       worker( type = "mix", dict = "inst/dict/jieba.dict.utf8",
##               hmm  = "inst/dict/hmm_model.utf8",  ### HMM模型数据
##               user = "inst/dict/user.dict.utf8") ### 用户自定义词库
fenci = mixseg <= "江州市长江大桥参加了长江大桥的通车仪式" ### <= 分词运算符
# 相当于 segment( "江州市长江大桥参加了长江大桥的通车仪式" , mixseg ) 
## 或者 mixseg["江州市长江大桥参加了长江大桥的通车仪式"]

#支持对文件名称进行分词：
mixseg <= "./temp.dat"  ### 自动判断输入文件编码模式，默认文件输出在同目录下。
## segment( "./temp.dat" , mixseg )


####################################################
#词典介绍

#可以自定义用户词库，推荐使用[深蓝词库转换]构建分词词库，它可以快速地将搜狗细胞词库等输入法词库转换为jiebaR的词库格式。

show_dictpath()     ### 显示词典路径
edit_dict("user")   ### 编辑用户词典
?edit_dict()        ### 打开帮助系统
#系统词典共有三列，第一列为词项，第二列为词频，第三列为词性标记。
#用户词典有两列，第一列为词项，第二列为词性标记。
#用户词库默认词频为系统词库中的最大词频，如需自定义词频率，可将新词添加入系统词库中。

############################################################
#词性标注
#词典中的词性标记采用ictclas的标记方法。
#可以使用 <=.tagger 或者 tag 来进行分词和词性标注，词性标注使用混合模型模型分词，标注采用和 ictclas 兼容的标记法。

words = "我爱北京天安门"
tagger = worker("tag")
tagged = tagger[words] #tagger <= words
str(tagged)
attr(tagged, "names")
data.frame(tag = attr(tagged, "names"),value = tagged)


############################################################
#关键词提取
#关键词提取所使用逆向文件频率（IDF）文本语料库可以切换成自定义语料库的路径，使用方法与分词类似。topn参数为关键词的个数。

keys = worker("keywords", topn = 1)
keys <= "我爱北京天安门"
keys <= "一个文件路径.txt"

############################################################
#Simhash 与海明距离

对中文文档计算出对应的simhash值。simhash是谷歌用来进行文本去重的算法，现在广泛应用在文本处理中。Simhash引擎先进行分词和关键词提取，后计算Simhash值和海明距离。

words = "hello world!"
simhasher = worker("simhash",topn=2)
simhasher <= "江州市长江大桥参加了长江大桥的通车仪式"

##############################################################
#快速模式
#无需使用worker()，使用默认参数启动引擎，并立即进行分词：

library(jiebaR)

qseg <= "江州市长江大桥参加了长江大桥的通车仪式"
#qseg ~ quick segmentation，使用默认分词模式，自动建立分词引擎，类似于ggplot2包里面的qplot函数。

### 第一次运行时，启动默认引擎 quick_worker，第二次运行，不再启动引擎。
qseg <= "这是测试文本。"
qseg

#可以通过qseg$重设模型参数，重设模型参数将会修改以后每次默认启动的默认参数，如果只是希望单次修改模型参数，可以使用非快速模式的修改方式quick_worker$。

qseg$type = "mp" ### 重设模型参数的同时，重新启动引擎。

qseg$type        ### 下次重新启动包是将使用现在的参数，构建模型。

quick_worker$detect = T ### 临时修改，对下次重新启动包时，没有影响。

get_qsegmodel()         ### 获得当前快速模式的默认参数

#######################################################################
#词典的使用
最简单的方法

打开 library/jiebaR/dict/ 目录下的 user.dict.utf8 文件（这个是默认载入的用户词典文件），直接往里面添加词就可以了。

打开后格式是这样子的

云计算
韩玉鉴赏
蓝翔 nz
CEO
江大桥 x
更多说明

用户词典有两列，第一列为词项，第二列为词性标记。用户词库默认词频为系统词库中的最大词频，如需自定义词频率，可将新词添加入系统词库中。

词典中的词性标记采用ictclas的标记方法。如果不知道怎么标记词性，可以不填写，直接导入词条就可以了。

比如你的词库只需要新建成如下：
摩尔定律
三角函数
内容存取存储器
操作系统

jieba.dict.utf8 是系统词典，共有三列，第一列为词项，第二列为词频，第三列为词性标记。

温馨提示

如果不使用另外的词典，新建worker的时候，可以不用指定词典路径，会自动查找，比如

> cutter = worker(type  = "mix", dict = "D:/Program Files/R/R-3.1.2/library/jiebaR/dict/jieba.dict.utf8",
                          hmm   = "D:/Program Files/R/R-3.1.2/library/jiebaR/dict/hmm_model.utf8",  
                          user  = "D:/Program Files/R/R-3.1.2/library/jiebaR/dict/user.dict.utf8",
                          detect=T,      symbol = F,
                          lines = 1e+05, output = NULL
                  )
#可以省略为
cutter = worker(type  = "mix")
如果指定用户词典目录，只需要写user这个参数就可以了，多写容易错，用默认就好。

> cutter = worker(type  = "mix"，user  = "D:/somefile.xxx")
制作词库

制作词库可以用 深蓝词库转换 或者 cidian 包。注意：如果使用 深蓝词库转换 导出有拼音项，将无法正常读取词库。

###########################################################################
#系统词典
可以动态地建立分词器：

worker( dict="系统词典，先加载，可以设置词频", 
        user="用户词典，系统词典加载后加载，所有词的词频为系统词典的最大词频")
如果需要添加词，直接打开记事本，然后一行输入一个词，就可以了。输入完以后，user=”刚才设置的文本文件的路径”，

用户词典不需要设置词频，是因为很有可能，用户也不太清楚一个词的词频是什么，但是加入用户词典的词，很有可能是要切分出来的，所以默认设置最大词频，保证这个词能够最大程度地被切出来，如果需要设置词频，可以把词加入到系统词典里。

比如user词库，在 C:/user.txt ：

北京大学
清华大学
新建分词器 cutter1 = worker(user = “C:/user.txt”)，

如果需要再加入几个大学，打开这个文件：

北京大学
清华大学
浙江大学
新建分词器 cutter2 = worker(user = “C:/user.txt”)，

这样，R 系统里就有两个分词器，可以动态地调用你想用的，对比不同分词器的结果。

######################################################
用户词典

可以自定义用户词库，推荐使用 深蓝词库转换 或者 分词词典工具 cidian 包 构建分词词库。它们可以快速地将搜狗细胞词库等输入法词库转换为jiebaR的词库格式。

show_dictpath()     ### 显示词典路径
edit_dict("user")   ### 编辑用户词典
?edit_dict()        ### 打开帮助系统
系统词典共有三列，第一列为词项，第二列为词频，第三列为词性标记。

用户词典有两列，第一列为词项，第二列为词性标记。用户词库默认词频为系统词库中的最大词频，如需自定义词频率，可将新词添加入系统词库中。

词典中的词性标记采用ictclas的标记方法。

#########################################################
#分词词典工具 cidian 包
安装

install.packages("devtools")#用于安装github包的驱动
install.packages("stringi")
install.packages("pbapply")
install.packages("Rcpp")
install.packages("RcppProgress")
library(devtools)
install_github("qinwf/cidian")
使用

decode_scel(scel = "细胞词库路径",output = "输出文件路径",cpp = TRUE)

decode_scel(scel = "细胞词库路径",output = "输出文件路径",cpp = FALSE,progress =TRUE)