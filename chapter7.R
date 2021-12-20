# 分散分析

# 一元配置分散分析
# データに含まれる因子（要因の中でも母平均に差をもたらすと考えられる研究対象）の数が一つのもの

setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

stat2_a <- df[df$method=="A",]$stat_test2
stat2_b <- df[df$method=="B",]$stat_test2
stat2_c <- df[df$method=="C",]$stat_test2
stat2_d <- df[df$method=="D",]$stat_test2

# 帰無仮説：４群の母平均は等しい(指導法の違いによる統計学の学習効果に差は無い)

# factor関数：要因型ベクトルに変更する
df_method <- factor(df$method)
df_method

oneway.test(df$stat_test2~df_method,var.equal = TRUE)
