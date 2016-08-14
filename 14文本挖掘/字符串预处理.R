setwd("/media/zhoutao/软件盘/workspace/R/machine learning/14文本挖掘")
#创建空字符串
empty_str = ""
class(empty_str)
#创建5个空字符串
empty_chr = character(5)
#空字符串长度为1
length(empty_str) 
#增加字符
example = character(0)
example[1] = "first"
example[4] = "fourth"
example

#判断字符串
a = "test me"
b = 8 + 9
is.character(a)
as.character(b)

##向量和矩阵中如果存在字符串，则其他元素也会变成字符串 
#向量中字符串会导致其他变量变成字符
c(1:5, pi, "text")
c(1:5, TRUE, pi, "text", FALSE)
c(1:5, TRUE, pi, "text", TRUE)
#数字与字符合并时称为矩阵，数字会被强制转换为字符
rbind(1:5, letters[1:5])

#数据框中，字符串会变成因子型数据
df1 = data.frame(numbers = 1:5, letters = letters[1:5])
df1
#字符串不变成因子
df2 = data.frame(numbers = 1:5, letters = letters[1:5],
                 stringsAsFactors = FALSE)
str(df2)

#列表的数据类型相互不干扰
list(1:5, letters[1:5], rnorm(5))

###################################字符串的操作##############################
#字符串的拼接
paste("The life of", pi)
IloveR = paste("I", "love", "R", sep = "-")
paste("X", 1:5, sep = ".")
paste(1:3, c("!", "?", "+"), sep = "", collapse = "")
paste(1:3, c("!", "?", "+"), sep = "")
evalue = paste("the value of 'e' is", exp(1), NA)
paste0("let's", "collapse", "all", "these", "words")

#打印字符串
my_string = "programming with data is fun"
print(my_string)
#qutoe不要引号
print(my_string, quote = FALSE)
#同上，不要引号
noquote(my_string)
no_quotes = noquote(c("some", "quoted", "text", "!%^(&="))
is.character(no_quotes)
no_quotes[2:3]

################cat直接打印字符串不带引号#################
#cat(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
cat(my_string)
cat(my_string, "with R")#追加字符串，相当于paste（）
cat(my_string, "with R", sep = " =) ")
cat(1:10, sep = "-")
cat(month.name[1:4], sep = " ")
#每个字符串换行输出
cat("Loooooooooong strings", "can be displayed", "in a nice format",
    "by using the 'fill' argument", fill = 30)
#打印内容并输出到output.txt 文件中去。
cat(my_string, "with R", file = "output.txt")

################format规范字符串的输出格式##################
format(13.7)
format(13.12345678)
format(13.7, nsmall = 3)#保留小数点后三位
format(c(6, 13.1), digits = 2)#保留2位数字，不足位数空格代替
format(c(6, 13.1), digits = 2, nsmall = 1)#
format(1/1:5, digits = 2)#同上对比较
#width整个字符串长度，justify位置
format(format(1/1:5, digits = 2), width = 6, justify = "c")

#位置的摆放
format(c("A", "BB", "CCC"), width = 5, justify = "centre")#字符串居中摆放
format(c("A", "BB", "CCC"), width = 5, justify = "left")
format(c("A", "BB", "CCC"), width = 5, justify = "right")
format(c("A", "BB", "CCC"), width = 5, justify = "none")

#数字用逗号间隔
format(123456789, big.mark = ",")

##############以C语言的格式输出#################
sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)#添加+号
sprintf("% f", pi)#左边空一个
sprintf("%-10f", pi)#-号左对齐
sprintf("%e", pi)#科学计数法


################toString将对象转化为字符###########################
toString(17.04)
toString(c("Bonjour", 123, TRUE, NA, log(exp(1))))
toString(c("one", "two", "3333333333"), width = 8)
toString(c("one", "two", "3333333333"), width = 12)


########################nchar()字符串中字符的个数##########################
#字符串中字符的位置
city = 'shanghai'
#nchar查看字符数量
nchar(city)
#which 取得位置
letter = c("a", "b", "d", "h")
which(letter =="d")
length(c("How", "many", "characters?"))#字符串长度
length("How many characters?")

######################casefold()转化大小写########################
tolower(c("aLL ChaRacterS in LoweR caSe", "ABCDE"))
toupper(c("All ChaRacterS in Upper Case", "abcde"))
#同上转化字符大小写
casefold("aLL ChaRacterS in LoweR caSe")
casefold("All ChaRacterS in Upper Case", upper = TRUE)#转化为大写
#####################substring()提取字符串#############################
#字符串提取
#提取取字符串substring()
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
#取每个字符串的2-5的字符
substr(x, first = 2, last = 5)
#2 分别对应4,5,6
substring(x,first = 2, last = 4:6)
#1与4对应 2与5对应 然后循环这个对应方式
substring(x, first=c(1,2), last = c(4,5))
#字符串替换
mystring = 'My name is Zhijie'
substring(mystring, 12, 17) = 'hong'


###################strsplit()拆分字符串############################
x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
#按e拆分 并返回列表， 用unlist 将列表转化为向量
x2 = strsplit(x, "e")
#use.names = FALSE 将列表向量化之后不使用列表名
x3 = unlist(x2, use.names = FALSE)
str = "one   two  three  four"
#以一个空格分隔
strsplit(str, ' ')
#去掉多余的空格,使用+号表示前面的空格有1到多个空格
strsplit(str, ' +')


#############################chartr()字符元素替换####################
chartr("a", "A", "This is a boring string")
crazy = c("Here's to the crazy ones", "The misfits", "The rebels")
chartr("aei", "#!?", crazy)

###############

################subsrt()########################
#取字符向量states每个字符串的1-4个字符 substr(x, start, stop)
substr(x = states, start = 1, stop = 4)
x = c("may", "the", "force", "be", "with", "you")
substr(x, 2, 2) <- "#"
y = c("may", "the", "force", "be", "with", "you")
substr(y, 2, 3) <- ":)"
z = c("may", "the", "force", "be", "with", "you")
substr(z, 2, 3) <- c("#", "@")

#######################substring(text, first, last = 1000000L)####################
substring("ABCDEF", 2, 4)
substr("ABCDEF", 2, 4)#效果相同
substring("ABCDEF", first = 1:6, last = 1:6)
text = c("more", "emotions", "are", "better", "than", "less")
#分别从1,2,3循环截取字符
substring(text, first = 1:3) 
#分别从1,2,3循环选取起始点，然后对应后面替换的字符串
substring(text, first = 1:3) <- c(" ", "zzz")
#################abbreviate()缩写字符串#####################
head(USArrests)
states = rownames(USArrests)
states2 = abbreviate(states)
#去掉每个字符串变量的名字，保留缩写的字符串 
names(states2) = NULL
#保留5个缩写字符
#abbreviate(names.org, minlength = 4, dot = FALSE, strict = FALSE,
#            method = c("left.keep", "both.sides"))
abbreviate(states, minlength = 5)
#得到字符串向量中每个字符串的长度
state_chars = nchar(states)
#获得最大长度的字符串
states[which(state_chars == max(state_chars))]


