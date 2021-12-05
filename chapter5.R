setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

# 統計的仮説検定
df[,'psych_test']

# 帰無仮説：μ＝12　心理学のテストの母平均は12である

# 統計検定量の実現値（z値）を求める
z_denominator <- mean(df[,'psych_test'])-12 # z値の分子を計算
z_numerator <- sqrt(10/length(df[,'psych_test'])) #z値の分母を計算
z_value <- z_denominator/z_numerator
