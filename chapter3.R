#この本で使うデータセットのダウンロード
setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

# 列名で抽出
df[,"stat_test1"]
df[,'stat_test2']

# 散布図
plot(df[,'stat_test1'],df[,'stat_test2'])
plot(df[,'psych_test'],df[,'stat_test1'])

# 共分散：偏差の積の平均
sum((df[,'stat_test1']-mean(df[,'stat_test1']))*(df[,'stat_test2']-mean(df[,'stat_test2'])))/length(df[,'stat_test1'])
# 不偏共分散
cov(df[,"stat_test1"],df[,"stat_test2"])

# 相関係数
cov(df[,"stat_test1"],df[,"stat_test2"])/(sd(df[,"stat_test1"])*sd(df[,'stat_test2']))
cor(df[,"stat_test1"],df[,"stat_test2"])

table(df[,'math'])
table(df[,'stat'])
# クロス統計
table(df[,'math'],df[,'stat'])

# ファイ係数
math_phi <- ifelse(df[,'math']=='好き',1,0)
math_phi
stat_phi <- ifelse(df[,'stat']=='好き',1,0)
stat_phi
cor(math_phi,stat_phi)
