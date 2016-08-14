setwd("/media/zhoutao/软件盘/workspace/R/machine learning/14文本挖掘")

#正则表达式
################################################
#正则表达式 替换
#sub() 仅替换第一次出现与规则表达式相匹配的字符串
#gsub() 对所有出现的与规则表达式相匹配的字符串进行替换

#pattern 正则表达式，即需要查找的包含规则字符串表达式的字符串
#replacement 对匹配的字符创进行内容替换
#x 字符型数据
#ignore.case = = FALSE 不区分大小写

txt2 <- "useRs may fly into JFK or laGuardia"
gsub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
sub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
#提取数据
txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")

#查找第几个字符串有gu 
i <- grep("[gu]", txt)
stopifnot( txt[i] == grep("[gu]", txt, value = TRUE)) 

#regexpr() gregexpr()
#regexpr()匹配结果记录第一次匹配的起始位置信息，不匹配则返回-1
re = regexpr("en", txt)
#抽取匹配的字符串, attr 取得re的属性match.length
#re是匹配的起始位置，re + attr(re, "match.length") -1表示最后的位置
res = substring(txt, re, re + attr(re, "match.length") -1)

#返回每个字符串的匹配信息，match.length属性对应与匹配字符的起始位置，精确
#记录了被匹配的字符串长度
gre = gregexpr("en", txt)
#使用mapply()函数处理gregexpr的输出
getexpr = function(str, greg){
  substring(str, grep, grep + attr(grep, "match.length") -1)
}
gres = mapply(FUN = getexpr, txt, gre)

#匹配带有k字符的字符串向量，
grep(pattern = "k", x = states, value = TRUE)
grep(pattern = "w", x = states, value = TRUE)
grep(pattern = "[wW]", x = states, value = TRUE)
#tolower()先将所有字符转化为小写，再匹配小w
grep(pattern = "w", x = tolower(states), value = TRUE)
grep(pattern = "W", x = toupper(states), value = TRUE)
#匹配时忽略大小写
grep(pattern = "w", x = states, value = TRUE, ignore.case = TRUE)

#对单词进行统计
hist(nchar(states), main = "Histogram",
     xlab = "number of characters in US State names")
#a在字符串向量中，出现在每个字符串元素的第几位，匹配的长度是多少，没有匹配的字符串元素则返回-1
positions_a = gregexpr(pattern = "a", text = states, ignore.case = TRUE)
#positions_a是列表，所有用sapply函数，返回一个向量，找到含有a的字符串
num_a = sapply(positions_a, function(x) ifelse(x[1] > 0, length(x), 0))
num_a
