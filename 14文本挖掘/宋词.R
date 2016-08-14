#对宋词进行统计分析
#目的：找出哪些词是词人经常用的词，从统计角度来讲就是
#计算一下宋词之中词语出现的频率，然后做一个排序就可以了。


#colClasses 确定列输出的变量类型
txt=read.csv("E:\\workspace\\R\\常用代码\\文本文件处理\\SongPoem\\SongPoem.csv",colClasses="character")
#edit(txt)

## 对txt$Sentence 用后面四个符号分隔字符串
sentences=strsplit(txt$Sentence,"，|。|！|？|、")
#str(sentences)
#unlist将列表转化为一个向量字符串
sentences=unlist(sentences)

#取出每个字符串中非空格的部分（空格也算一个字符）
sentences=sentences[sentences!=""]

#查看每个字符串的字符长度，
s.len=nchar(sentences)

# 单句太长了说明有可能是错误的字符，去除掉。
sentences=sentences[s.len<=15];
s.len=nchar(sentences);


#例如《青玉案》中的这句“东风夜放花千树”
#两个字组合就是 ：东风  风夜  夜放  放花  花千  千树
#三个字组合： 东风夜  风夜放  夜放花  放花千  花千树
#这其中会有很多无意义的组将不太可能大规模地重复出现，
#因此在排序的过程中它们自动地就被排在高频词语之后了
##按照之前的做法把所有可能的字的组合计算出来。这里只是考虑了两个字的组合。
#substring(text, first, last ) 后面是1对应2； 2对应3。。。len-1 对应len
splitwords=function(x,x.len) substring(x, 1:(x.len-1), 2:x.len)

#sentences, s.len 分别对应splitwords中两个参数
#SIMPLIFY= TRUE 输出结果转化为向量矩阵高维数组   
#USE.NAMES=FALSE 如果字符串为向量 是否要加名称 
words=mapply(splitwords, sentences, s.len, SIMPLIFY=TRUE, USE.NAMES=FALSE);

#再次组合为一个字符串向量
words=unlist(words)

#计算每个词组的频数
words.freq=table(words)

#将词组按频数的降序排列
words.freq=sort(words.freq,decreasing=TRUE)

words.freq[1:100]
