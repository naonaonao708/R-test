# 二つの平均値を比較する

setwd('~/Desktop/R-test')
df <- read.csv('./data/shidouhouU8.csv')

# 性別が男の統計テスト１の値
stat1_m <- df[df$sex=="男", ]$stat_test1
stat1_f <- df[df$sex=="女", ]$stat_test1

# 帰無仮説：二つの母平均は等しい

mean(stat1_m)
mean(stat1_f)
var(stat1_m)
var(stat1_f)

pooled_sd <- sqrt(((length(stat1_m)-1)*var(stat1_m)+(length(stat1_f)-1)*var(stat1_f))/(length(stat1_m)+length(stat1_f)-2))
pooled_sd

t_value <- (mean(stat1_m)-mean(stat1_f))/(pooled_sd*sqrt(1/length(stat1_m)+1/length(stat1_f)))
t_value

qt(0.025,18)
qt(0.025,18,lower.tail = FALSE)
# 検定統計量が棄却域に入っていないため、帰無仮説は棄却されない
# 5%水準で有意差は見られなかった

2*pt(-1.842885,18)
# p値が有意水準5％よりも大きいので帰無仮説は棄却されない

t.test(stat1_m,stat1_f,var.equal = TRUE)
# var.equal=TRUEのオプションをつけないとt検定ではなくWelchの検定になってしまう

# 得点の変化の母平均は０である
vari_stat <- df[,'stat_test2'] - df[,'stat_test1']
t_value <- mean(vari_stat)/(sd(vari_stat)/sqrt(length(vari_stat)))
t_value
qt(0.025,19)
qt(0.025,19,lower.tail = FALSE)

t.test(vari_stat)
# 対応のあるt検定
t.test(df[,'stat_test1'],df[,'stat_test2'],paired = TRUE)

