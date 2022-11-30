
# library("SciViews")

setwd('C://Users//Saleem//OneDrive - Technion//Data science//semester 3//statistics 1 - 094424//homeworks//hw4//hw4 ref')
file = read.csv('Optime.csv')
data = file$optime
n = length(data)

hist(data, freq=FALSE)

# Question 3 D
mean = sum(log(data, base=exp(1))) / n
var = sum((log(data, base=exp(1)) - mean)^2) / n
mean
var

# Question 3 F

NormalQQ = function(data, mean, var) {
  indcies = (1:n - 0.5) / n
  norm.q = qnorm(indcies, mean=mean, sd=sqrt(var))
  log.data = log(sort(data))
  qqnorm(log.data, mean=mean, sd=sqrt(var))
  abline(mean, sqrt(var), col='red')
}

NormalQQ(data.sorted, mean, var)



LogNormQQ = function(data, mean, var) {
  indcies = (1:n - 0.5) / n
  norm.q = qnorm(indcies, mean=mean, sd=sqrt(var))
  lognorm.q = exp(norm.q)
  plot(lognorm.q, data)
  abline(0, 1, col='green')
}

LogNormQQ(data.sorted, mean, var)



hist(data, breaks=15, freq=FALSE, col='light blue')

M1 = mean(data)
M2 = sum(data^2) / n

b = sum(data) / sum((data - M1) ^ 2)
a = (sum(data))^2 / (n * sum((data - M1) ^ 2))

x = seq(0, 8, 0.1)
lines(x, dlnorm(x, mean=mean, sd=sqrt(var)), col='red')

lines(x, dgamma(x, a, b), col='dark green')





sum(data > 2) / n





