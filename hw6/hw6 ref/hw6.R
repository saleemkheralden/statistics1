

N = 10000
n = 100


mu_x = 2
mu_y = 2
sigma = 3

W = rep(0, N)
M = rep(0, N)

S = function(sampleX, sampleY) {
  return((sum((sampleX - mean(sampleX))^2) + sum((sampleY - mean(sampleY))^2)) / (length(sampleX) + length(sampleY) - 2))
}

for (i in 1:N) {
  sampleX = rnorm(n, mean=mu_x, sd=sigma)
  sampleY = rnorm(n, mean=mu_y, sd=sigma)
  
  W[i] = (2 * n - 2) * S(sampleX, sampleY) / sigma^2  
  M[i] = ((mean(sampleY) - mean(sampleX)) - (mu_y - mu_x)) / sqrt(S(sampleX, sampleY) * 2 / n)
}

interW = seq(0, max(W), 0.01)
hist(W, freq=FALSE, col='light blue', breaks=20)
lines(interW, dchisq(interW, 2 * n - 2), col='red')
lines(interW, dnorm(interW, mean=2*n-2, sd=sqrt(2 * (2*n-2))), col='dark green')

interM = seq(min(M), max(M), 0.01)
hist(M, freq=FALSE, col='light blue', breaks=20, ylim=c(0, 0.5))
lines(interM, dnorm(interM), col='dark green')

pchisq(3 * n - 3, 2 * n - 2) - pchisq(n - 1, 2 * n - 2)



x = seq(-3, 3, 0.01)
nn = x^2
n2n1 = (2 * x - 1) * (x - 1)

plot(x, nn, type='l', col='red')
lines(x, n2n1)







