# プリポストデザインデータの分析

setwd('~/Desktop/R-test')
prepost <- read.csv('./data/prepost.csv')

# attach関数：DFの値を項目名だけで検索できる
attach(prepost)
pre

# 共分散分析
summary(lm(post~pre+group))
summary(aov(post~pre+group))

# 回帰係数の等質性の検定
summary(aov(post~pre*group))


# 変化量について、t検定によって群の効果を吟味
variation <- post-pre

var.test(variation~group)
t.test(variation~group,var.equal=TRUE)


# 回帰分析
regression <- read.csv('./data/city62.csv')
regression

attach(regression)

summary(lm(POPULAT~REGION+CITYAGE+FUNCTION))

