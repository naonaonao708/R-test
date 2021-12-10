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


# t分布を用いた検定

# 帰無仮説：μ＝12

t_denominator <- mean(df[,'psych_test']) - 12
t_numerator <- sqrt(var(df[,'psych_test'])/length(df[,'psych_test']))
t_value <- t_denominator/t_numerator

qt(0.025,19)
qt(0.025,19,lower.tail = FALSE)
# t統計量が棄却域に入るため、帰無仮説は棄却

2*pt(2.616648,19,lower.tail = FALSE) # p値を計算
# p値が有意水準よりも小さいため帰無仮説は棄却される

t.test(df[,'psych_test'],mu=12) # Rでは一行でT.TESTができる


# 相関係数の検定（無相関検定）
df[,'stat_test1']
df[,'stat_test2']

# 帰無仮説：ρ＝０（母相関は０である）

sample_corr <- cor(df[,'stat_test1'],df[,'stat_test2'])
t_value_cor <- sample_corr*sqrt(length(df[,'stat_test1'])-2)/sqrt(1-sample_corr^2)

qt(0.025,18)
qt(0.025,18,lower.tail = FALSE)
2*pt(4.8057,18,lower.tail = FALSE)
# 検定統計量が棄却域に入り、p値が有意水準0.05よりも小さいので帰無仮説は棄却される

cor.test(df[,'stat_test1'],df[,'stat_test2']) # Rでの無相関検定
# ここではピアソンの積率相関係数を使っている


# 独立性の検定（カイ二乗検定）

# 帰無仮説：二つの変数は独立である

