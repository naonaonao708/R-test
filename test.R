height <- c(173,165,187,168,180,175,179)
height
# Rでは＝代わりに<-を使う

setwd('~/Desktop/R-test')
wine_df <- read.csv('./data/winemag-data_first150k.csv')
wine_df
# setwdでdirectoryを指定してread.csvする

# 標本分散を求める関数
varp <- function(x) {
  dt <- var(x)*(length(x)-1)/length(x)
  dt
}

x <- c(10,13,8,15,8)
x
var(x)
varp(x)

test_a <- c(10,13,8,15,8)
test_a

sum(test_a)
mean(test_a)
# 度数分布
table(test_a)

var(test_a)
