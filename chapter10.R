# 外れ値が相関係数に及ぼす影響

setwd('~/Desktop/R-test')
df <- read.csv('./data/animal.csv')

plot(df$Body.weight..kg.,df$Brain.weight..g.)
cor(df$Body.weight..kg.,df$Brain.weight..g.)

df_2 <- df[(df$Body.weight..kg.<2000) & (df$Brain.weight..g.<1000),]
plot(df_2$Body.weight..kg.,df_2$Brain.weight..g.)
cor(df_2$Body.weight..kg.,df_2$Brain.weight..g.)


# 統計解析で分かること・分からないこと

x <- c(290,50,80,100,200,350,430,80,210,110,70,260,220,330,170,420,80,300,290,230)
y <- c(350,70,100,130,250,430,520,100,260,140,90,320,270,400,210,510,100,370,350,280)

par(mfrow=c(2,1))
hist(x,breaks = c(0,60,120,180,240,300,360,420,480,540,600))
hist(y,breaks = c(0,60,120,180,240,300,360,420,480,540,600))

mean(x)
median(x)

varp <- function(x) {
  dt <- var(x)*(length(x)-1)/length(x)
  dt
}

varp(x)
varp(y)
