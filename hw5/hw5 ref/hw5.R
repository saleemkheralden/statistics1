
set.seed(9581)

N = 2000
n = c(10, 100, 1000)

a = 4.3
b = 1.8

p1 = pgamma(4, a, b)
p2 = 1- pgamma(8, a, b)

a_moment_vec = matrix(0, length(n), N)
b_moment_vec = matrix(0, length(n), N)
a_mle_vec = matrix(0, length(n), N)
b_mle_vec = matrix(0, length(n), N)
p1_moment_vec = matrix(0, length(n), N)
p2_moment_vec = matrix(0, length(n), N)
p1_mle_vec = matrix(0, length(n), N)
p2_mle_vec = matrix(0, length(n), N)

A_moment = function(sample) {
  s.len = length(sample)
  X = mean(sample)
  return(sum(sample)^2 / (s.len * sum((sample - X)^2)))
}

B_moment = function(sample) {
  X = mean(sample)
  return(sum(sample) / sum((sample - X)^2))
}

A_MLE = function(sample) {
  d <- 1
  a.moment <- A_moment(sample)
  
  sum.log <- sum(log(sample))
  len.sample <- length(sample)
  mean.s <- mean(sample)  
  dif.gam <- function(alpha) {sum.log + len.sample * log(alpha / mean.s) - len.sample * digamma(alpha)}
  for (i in 1:100) {
    if (dif.gam(i) > 0 && dif.gam(i + 1) < 0 ||
        dif.gam(i) < 0 && dif.gam(i + 1) > 0) {
      left_ep = i
      right_ep = i + 1
      break
    }
  }
  return(uniroot(dif.gam, c(left_ep, right_ep))$root)
}

B_MLE = function(sample) {
  a = A_MLE(sample)
  return(a / mean(sample))
}

p1_moment = function(a.moment, b.moment) {
  return(pgamma(4, a.moment, b.moment))
}

p2_moment = function(a.moment, b.moment) {
  return(1 - pgamma(8, a.moment, b.moment))
}

p1_mle = function(a.mle, b.mle) {
  return(pgamma(4, a.mle, b.mle))
}

p2_mle = function(a.mle, b.mle) {
  return(1 - pgamma(8, a.mle, b.mle))
}

for (i in 1:length(n)) {
  for (j in 1:N) {
    sample = rgamma(n[i], a, b)
    a_moment_vec[i, j] = A_moment(sample)
    b_moment_vec[i, j] = B_moment(sample)
    a_mle_vec[i, j] = A_MLE(sample)
    b_mle_vec[i, j] = B_MLE(sample)
    p1_moment_vec[i, j] = p1_moment(a_moment_vec[i, j], b_moment_vec[i, j])
    p2_moment_vec[i, j] = p2_moment(a_moment_vec[i, j], b_moment_vec[i, j])
    p1_mle_vec[i, j] = p1_mle(a_mle_vec[i, j], b_mle_vec[i, j])
    p2_mle_vec[i, j] = p2_mle(a_mle_vec[i, j], b_mle_vec[i, j])
  }
}

for (index in 1:3) {
  cat('\n\n\nn =', index ,'\n')
  print(a)
  print(mean(a_moment_vec[index, ]))
  print(mean(a_mle_vec[index, ]))
  
  print(b)
  print(mean(b_moment_vec[index, ]))
  print(mean(b_mle_vec[index, ]))
  
  print(p1)
  print(mean(p1_moment_vec[index, ]))
  print(mean(p1_mle_vec[index, ]))
  
  print(p2)
  print(mean(p2_moment_vec[index, ]))
  print(mean(p2_mle_vec[index, ]))
}





#--------------------------------------
# table

bias = function(vector, theta) {
  return(mean(vector) - theta)
}

MSE = function(vector, theta) {
  return(var(vector) - bias(vector, theta)^2)
}


MSE(p2_mle_vec[1, ], p2)
MSE(p2_mle_vec[2, ], p2)
MSE(p2_mle_vec[3, ], p2)



