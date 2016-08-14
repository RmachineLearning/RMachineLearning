#R语言与数据挖掘最佳实践和经典案例

#第10章 文本挖掘
library(twitteR)

rdmTweets <- userTimeline("rdatamining", n=200)
ut <- userTimeline('barackobama', n=100)
(nDocs <- length(rdmTweets))
rdmTweets[11:15]

for (i in 11:15) {
   cat(paste("[[", i, "]] ", sep=""))
   writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))
   }