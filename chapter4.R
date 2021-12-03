# 確率分布
# ceiling()：小数点以下の切り上げ→切り下げはfloor関数を使う
# runif(n, min, max)：一様分布に従う乱数データを発生させる
dice_100 <- ceiling(runif(n=100,min=0,max=6))
table(dice_100)

# 母集団分布
barplot(c(2/3,1/3),names.arg=c("man","woman"))

# 正規分布
# curve(xを含んだ関数式, from=左端の値, to=右端の値)：指定した関数のグラフを作成する
# dnorm(確率変数の値、平均、標準偏差)：確率密度関数を求める関数
curve(dnorm(x,mean=0,sd=1),from=-4,to=4)

# rnorm(n, mean, sd)：標準正規分布に従う乱数を無作為に抽出する
rnorm(n=5,mean=50,sd=10)
hist(rnorm(n=100000,mean=50,sd=10))

# 標本分布をモンテカルロシミュレーションで求める
sample_mean <- numeric(length=10000)　# 推定値を格納する場所を予約
for(i in 1:10000){
  sample_test <- rnorm(n=10,mean=50,sd=10)
  sample_mean[i] <- mean(sample_test) # 標本平均の計算
}
hist(sample_mean)

# そのままヒストグラムと正規分布を重ねると縦軸の値が異なりずれるため、freq=FALSEでヒストグラムの縦軸の単位を揃える
hist(sample_mean,freq=FALSE)
curve(dnorm(x,mean=50,sd=sqrt(10)),add=TRUE)

# N(50,10^2)の正規母集団からn=20の標本抽出を5000回繰り返す
test_mean <- numeric(length=5000)

for(i in 1:5000){
  tes <- rnorm(n=20,mean=50,sd=10)
  test_mean[i] <- mean(tes)
}

hist(test_mean,freq=FALSE)
curve(dnorm(x,mean=50,sd=sqrt(100/20)),add=TRUE)

# 理論的な標本分布についてサンプルサイズをn=1,4,9,16,25と変化させる　N(0,1^2)
curve(dnorm(x,mean=0,sd=sqrt(1/25)),-3,3)
curve(dnorm(x,mean=0,sd=sqrt(1/16)),-3,3)
curve(dnorm(x,mean=0,sd=sqrt(1/9)),-3,3)
curve(dnorm(x,mean=0,sd=sqrt(1/4)),-3,3)
curve(dnorm(x,mean=0,sd=sqrt(1/1)),-3,3)
