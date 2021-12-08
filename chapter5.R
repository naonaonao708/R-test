setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

# 統計的仮説検定
df[,'psych_test']

# 帰無仮説：μ＝12　心理学のテストの母平均は12である

# 統計検定量の実現値（z値）を求める
z_denominator <- mean(df[,'psych_test'])-12 # z値の分子を計算
z_numerator <- sqrt(10/length(df[,'psych_test'])) #z値の分母を計算
z_value <- z_denominator/z_numerator

# qnorm(p)：第一引数に確率を受けて、確率密度を返す
qnorm(0.025) # 下側確率0.025となるz値を求める→両側検定・有意水準５％
qnorm(0.975) # 下側確率0.975となるz値

qnorm(0.025, lower.tail = FALSE) #上側確率0.025のz値

# 今回の棄却域であるZ＜pnorm(0.025),Z＞pnorm(0.975)にZ値が入るため、帰無仮説は棄却される

# pnorm(q)：第一引数に確率密度を受けて、確率を返す
pnorm(-2.828427) # 下側確率Prob(z=<-2.828427)
pnorm(2.828427,lower.tail = FALSE) # 上側確率Prob(z>2.828427)

2*pnorm(2.82847,lower.tail = FALSE) # 両側検定なので＊２
# 値が0.004677108と有意水準である0.05よりも小さいので帰無仮説は棄却される

