#字符处理
setwd("/media/zhoutao/软件盘/workspace/R/machine learning/14文本挖掘")

#使用包来解决字符串上述的问题
library(stringr)
#合并字符串
str_c("May", "The", "Force", "Be", "With", "You")
str_c("May", "The", "Force", NULL, "Be", "With", "You", character(0))

######################联合字符串str_join()############################
str_join("May", "The", "Force", "Be", "With", "You", sep = "-")
#字符长度
str_length(some_text)
some_factor = factor(c(1, 1, 1, 2, 2, 2), labels = c("good", "bad"))
str_length(some_factor)

####################字符串的截取str_sub(string, start = 1L, end = -1L)############
lorem = "Lorem Ipsum"
str_sub(lorem, start = 1, end = 5)
substring(lorem, first = 1, last = 5)
str_sub(lorem, 1, 5) <- "Nullam"
str_sub(lorem, c(1, 7), c(5, 8)) <- c("Nullam", "Enim")

######################str_dup(string, times)重复次数#########################
str_dup("hola", 3)
words = c("lorem", "ipsum", "dolor", "sit", "amet")
str_dup(words, 1:5)

################str_pad("hola", width = 7)############################
str_pad("hola", width = 7)#长度
str_pad("adios", width = 7, side = "both")#两端对其
str_pad("hashtag", width = 8, pad = "#")
str_pad("hashtag", width = 9, side = "both", pad = "-")

#########################str_wrap(string, width = 80, indent = 0, exdent = 0)############
some_quote = c(
  "I may not have gone",
  "where I intended to go,",
  "but I think I have ended up",
  "where I needed to be")
some_quote = paste(some_quote, collapse = " ")
cat(str_wrap(some_quote, width = 30))
cat(str_wrap(some_quote, width = 30, indent = 2), "\n")
cat(str_wrap(some_quote, width = 30, exdent = 3), "\n")

############str_trim(string, side = "both")去掉空格#######################
bad_text = c("This", " example ", "has several", "whitespaces ")
str_trim(bad_text, side = "left")#去掉左边空格
str_trim(bad_text, side = "right")
str_trim(bad_text, side = "both")

##############word(string, start = 1L, end = start, sep = fixed(" "))选择元素#######
change = c("Be the change", "you want to be")
word(change, 1)
word(change, -1)
word(change, 2)
word(change, 2, -1)


#计算字符串的字符个数
str_count(states, "a")
str_count(tolower(states), "a")
vowels = c("a", "e", "i", "o", "u")
num_vowels = vector(mode = "integer", length = 5)
#得带每个元素的位置
seq_along(vowels)

for (j in seq_along(vowels)) {
  num_aux = str_count(tolower(states), vowels[j])
  num_vowels[j] = sum(num_aux)
}
sort(num_vowels, decreasing = TRUE)

barplot(num_vowels, main = "Number of vowels in USA States names",
        border = NA, ylim = c(0, 80))
