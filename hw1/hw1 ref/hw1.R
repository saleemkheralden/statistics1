




setwd('C://Users//SLIMS-PC//OneDrive - Technion//Data science//semester 3//statistics 1 - 094424//homeworks//hw1//hw1 ref')
file = read.csv('Prices.csv')

hist(file$Price, breaks=seq(0.1, 7.1, 0.1), xlim=c(0.1, 7.1), freq=FALSE, col='green')

X = sort(file$Price)
P = rep(0, length(X))

p_i = (1:length(P) - 0.5)/length(P)

P = qnorm(p_i)

plot(P, X)

qqnorm(file$Price)
abline(mean(X), sd(X), col='red')


qqline(file$Price)

boxplot(X, horizontal=TRUE)



step = 1.5 * (Q3 - Q1)
step

LF = Q1 - step
UF = Q3 + step

LF
UF

LW = 0
for (i in 1:length(X)) {
  if (X[i] >= LF) {
    LW = X[i]
    break
  }
}

UW = 0
for (i in length(X):1) {
  if (X[i] <= UF) {
    UW = X[i]
    break
  }
}

as.numeric(UF)

UW
LW

table(X)

outter = sum(X < LF | X > UF)
outter

X

mean(X)
median(X)
Q1 = quantile(X, prob=0.25)
Q3 = quantile(X, prob=0.75)
Q1
Q3
var(X)
sd(X)
max(X) - min(X)
r = Q3 - Q1
r

################################################



# Qes 2
# A

set.seed(9581)
sample1 = rexp(5, 0.2)
sample2 = rexp(20, 0.2)
sample3 = rexp(500, 0.2)

mean1 = mean(sample1)
mean2 = mean(sample2)
mean3 = mean(sample3)

var1 = var(sample1)
var2 = var(sample2)
var3 = var(sample3)

med1 = median(sample1)
med2 = median(sample2)
med3 = median(sample3)

p = 0.25 # Q1
Q_p1 = quantile(sample1, prob=p)
Q_p2 = quantile(sample2, prob=p)
Q_p3 = quantile(sample3, prob=p)

print(mean1)
print(var1)
print(med1)
print(Q_p1)

print(mean2)
print(var2)
print(med2)
print(Q_p2)

print(mean3)
print(var3)
print(med3)
print(Q_p3)



# D

x = seq(0, max(sample3), 0.1)
plot(x, pexp(x, 0.2), type='l', col='red')
lines(ecdf(sample1), col='blue')
lines(ecdf(sample2), col='black')
lines(ecdf(sample3), col='green')



























