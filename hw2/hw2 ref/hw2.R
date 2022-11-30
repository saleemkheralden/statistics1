
set.seed(81 + 97)

N = 3000
n = 10
p = 0.2


mat = matrix(0, n, N)

for (i in 1:N) {
  sample = 1 + rgeom(n, p)
  mat[,i] = sample
}

t1 = rep(0, N)
t2 = rep(0, N)
t3 = rep(0, N)
m = rep(0, N)

T1 = function (arg1) {
  temp1 = mean(arg1)
  return(temp1*(temp1 - 1))
}

T2 = function(arg2) {
  mean2 = mean(arg2)
  temp2 = arg2 - mean2
  temp2 = temp2 ^ 2
  return(sum(temp2) / length(temp2))
}

T3 = function (arg3) {
  temp3 = arg3 ^ 2
  return((sum(temp3) / (2*length(temp3))) - 0.5 * mean(arg3))
}


for (i in 1:N) {
  t1[i] = T1(mat[, i])
  t2[i] = T2(mat[, i])
  t3[i] = T3(mat[, i])
  m[i] = mean(mat[, i])
}

mean(t1)
mean(t2)
mean(t3)


hist(t1, freq=FALSE, ylim=c(0, 0.05), col=c1)
hist(t2, freq=FALSE, col=c2)
hist(t3, freq=FALSE, col=c3)



# P(|max(T1) - Var(x)| <= d) <= Var(max(T1)) / d^2



# P(max(T1) < )











snow <- c(1,1,3,3,3,4,5,5,5,5,5,5,5,5,6,10,10,10,15,15,15,15,20,20,20,20,20, 20,30,30,35,40,40,40,40,45,65)
n.snow = length(snow)

sqrt(T1(snow))
sqrt(T2(snow))
sqrt(T3(snow))

sqrt((n.snow/(n.snow+1)) * T1(snow))
sqrt((n.snow/(n.snow-1)) * T2(snow))
sqrt(T3(snow))





























