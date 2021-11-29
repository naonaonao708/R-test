# 2章練習問題　P54
a_univ <- c(60,100,50,40,50,230,120,240,200,30)
b_univ <- c(50,60,40,50,100,80,30,20,100,120)

hist(a_univ)
hist(b_univ)
mean(a_univ)
sqrt(mean((a_univ-mean(a_univ))^2))
mean(b_univ)
sqrt(mean((b_univ-mean(b_univ))^2))
(a_univ-mean(a_univ))/sqrt(mean((a_univ-mean(a_univ))^2))*10+50
(b_univ-mean(b_univ))/sqrt(mean((b_univ-mean(b_univ))^2))*10+50

a <- c(1.116,1.693,1.723,1.78,1.834,1.796,1.789,1.666)
1300*a-1300
rm(a)

