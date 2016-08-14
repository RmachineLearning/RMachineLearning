setwd("/media/zhoutao/软件盘/workspace/R/machine learning/14文本挖掘")

#字符串的集合运算
set1 = c("some", "random", "words", "some")
set2 = c("some", "many", "none", "few")
union(set1, set2)#求et4于set3的并集

set3 = c("some", "random", "few", "words")
set4 = c("some", "many", "none", "few")
intersect(set3, set4)#求set4于set3的交集

set5 = c("some", "random", "few", "words")
set6 = c("some", "many", "none", "few")
setdiff(set5, set6)#求set5不同与set6的集合

#判断集合是否相同
set7 = c("some", "random", "strings")
set8 = c("some", "many", "none", "few")
set9 = c("strings", "random", "some")
setequal(set7, set8)
setequal(set7, set9)
#效果同上
identical(set7, set7)
identical(set7, set9)

#判断是否在集合中
elem1 = c("some", "stuff", "to", "play", "with")
elem2 = "play"
vectors  = "crazy"
is.element(elem1, set10)
elem1 %in% set10#效果同上

#排序sort
set11 = c("today", "produced", "example", "beautiful", "a", "nicely")
sort(set11)
sort(set11, decreasing = TRUE)



