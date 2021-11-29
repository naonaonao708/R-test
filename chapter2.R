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
table(test_a) #度数分布
var(test_a) #不偏分散
sd(test_a) #標準偏差

# 平均偏差：平均からの偏差の絶対値の平均
mean(abs(test_a-mean(test_a)))

test_b <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)
mean(test_b)

# 標本分散で標準偏差を求める
# sd関数では不偏分散の平方根で計算されるので、sqrt(mean(x-x_mean)^2)で求める
test_b_sd <- sqrt(mean((test_b-mean(test_b))^2))
test_b_sd

# z得点＝（データー平均）/標準偏差
# z得点：平均０、標準偏差１に変換した標準得点
test_b_z <- (test_b-mean(test_b))/test_b_sd
test_b_z

# 偏差値＝z得点*10+50
# 偏差値：平均50、標準偏差10になるように標準化した標準得点
test_b_t <- test_b_z*10+50
test_b_t

