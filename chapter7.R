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
# 一元配置分散分析のみ実行

aov(df$stat_test2~df_method)
summary(aov(df$stat_test2~df_method))
# 分散分析表を表示

anova(lm(df$stat_test2~df_method))
# 複数のモデルの比較など高度な分析に対応

# cbind()：横方向に繋いで行列を作る関数
all_data <- cbind(stat2_a,stat2_b,stat2_c,stat2_d)
all_data

# colMeans()：行列の列ごとの平均を求める関数
colMeans(all_data)

all_data - mean(all_data)
colMeans(all_data) - mean(all_data)
all_data - colMeans(all_data)

# 全体平方和
sum((all_data - mean(all_data))^2)

# 群間平方和
sum(rep((colMeans(all_data) - mean(all_data)),4)^2)


# 多重比較（Tukeyの方法）

# 帰無仮説：４群の平均は等しい


q <- abs(mean(stat2_a)-mean(stat2_d))/sqrt(8.625/nrow(all_data))
q

qtukey(0.95,4,16)
qtukey(0.05,4,16,lower.tail = FALSE)
