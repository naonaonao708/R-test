# 二つの平均値を比較する

setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

# 性別が男の統計テスト１の値
df[df$sex=="男", ]$stat_test1
