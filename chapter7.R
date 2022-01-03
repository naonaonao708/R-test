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

# Tukeyによる多重比較
TukeyHSD(aov(df$stat_test2~df_method))
# A-CとA-Dで有意差があると言える


# 一元配置分散分析（対応あり）

favor <- c(7,8,9,5,6,5,4,7,1,3,8,6,7,2,5)
subject <- factor(c(rep("線形代数",5),rep("微分積分",5),rep("確率統計",5)))

summary(aov(favor~subject))
# 対応のあるデータで対応のあることを考慮しないで検定を行うと、検定力が低下して有意な結果が得られにくくなる。
# 対応を考慮した分析を行う

# 帰無仮説：三科目の好意度の母平均は等しい
# 検定統計量はF＝（条件平方和／条件の自由度）／（残差平方和／残差の自由度）
people <- factor(rep(c("田中","岸","大引","吉川","荻野"),3))
summary(aov(favor~subject+people))
# F値＝14.696

qf(0.05,2,8,lower.tail = FALSE)
# F値が棄却域に入るため帰無仮説は棄却される

all_data <- matrix(c(7,8,9,5,6,5,4,7,1,3,8,6,7,2,5),nrow=5,ncol=3)

colMeans(all_data)
rowMeans(all_data)

all_matrix <- matrix(rep(mean(all_data),15),nrow=5,ncol=3)
all_matrix

subject_matrix <- matrix(rep(colMeans(all_data),5),nrow=5,ncol=3,byrow=TRUE)
subject_matrix

people_matrix <- matrix(rep(rowMeans(all_data),3),nrow=5,ncol=3)
people_matrix

# 全体平方和
all_ss <- sum((all_data-all_matrix)^2)
all_ss

# 条件平方和
condition_ss <- sum((subject_matrix-all_matrix)^2)
condition_ss

# 個人差平方和
individual_ss <- sum((people_matrix-all_matrix)^2)
individual_ss

# 残差平方和
residual_ss <- sum((all_data-all_matrix-(subject_matrix-all_matrix)-(people_matrix-all_matrix))^2)
residual_ss

# 一元配置分散分析（対応あり）では全体平方和＝条件平方和＋個人平方和＋残差平方和が成り立つ


# 二元配置分散分析（対応なし）

# 要因Aの主効果　帰無仮説：温度が違っても美味しさ得点の母平均は等しい
# 要因Bの主効果　帰無仮説：銘柄が違っても美味しさ得点の母平均は等しい
# 要因A、Bの交互作用効果　帰無仮説：温度と銘柄の組み合わせに相性の良し悪しはない

taste <- c(6,4,5,3,2,10,8,10,8,9,11,12,12,10,10,5,4,2,2,2,7,6,5,4,3,12,8,5,6,4)
temperature <- factor(c(rep("冷蔵",15),rep("常温",15)))
brand <- factor(rep(c(rep("イカアン",5),rep("ボズビック",5),rep("ビビッテル",5)),2))

summary(aov(taste~temperature*brand))

# 交互作用効果は要因を：で結んで表す
summary(aov(taste~temperature+brand+temperature:brand))

# 温度と銘柄での主効果は有意差が見られるが、交互作用効果は見られない

interaction.plot(temperature,brand,taste)
interaction.plot(brand,temperature,taste)


# 二元配置分散分析（対応あり）
num_id <- factor(rep(1:5,6))

summary(aov(taste~temperature*brand+Error(num_id+num_id:temperature+num_id:brand+num_id:temperature:brand)))


# 二元配置分散分析（１要因のみ対応あり）
num_id2 <- factor(c(rep(1:5,3),rep(6:10,3)))

summary(aov(taste~temperature*brand+Error(num_id2:temperature+num_id2:temperature:brand)))
