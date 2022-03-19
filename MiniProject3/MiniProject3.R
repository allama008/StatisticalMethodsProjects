# MiniProject 3

# Question 1
# Function to return MLE and MoM of one sample based on n and theta
MLEMOM <- function(n, theta) {
  sample = runif(n, min = 0, max = theta)
  mle = max(sample)
  mom = 2*mean(sample)
  return (c(mle, mom))
}

MLEMOM(1, 1)

MSE <- function(n, theta, rep){
  samples = replicate(rep, MLEMOM(n, theta))
  estimates = (samples - theta)^2
  return (c(mean(estimates[1, ]), mean(estimates[2, ])))
}

# n = 1
MSE_1_1 = MSE(1,1,1000)
MSE_1_5 = MSE(1,5,1000)
MSE_1_50 = MSE(1,50,1000)
MSE_1_100 = MSE(1,100,1000)

# n = 2
MSE_2_1 = MSE(2,1,1000)
MSE_2_5 = MSE(2,5,1000)
MSE_2_50 = MSE(2,50,1000)
MSE_2_100 = MSE(2,100,1000)

# n = 3
MSE_3_1 = MSE(3,1,1000)
MSE_3_5 = MSE(3,5,1000)
MSE_3_50 = MSE(3,50,1000)
MSE_3_100 = MSE(3,100,1000)

# n = 5
MSE_5_1 = MSE(5,1,1000)
MSE_5_5 = MSE(5,5,1000)
MSE_5_50 = MSE(5,50,1000)
MSE_5_100 = MSE(5,100,1000)

# n = 10
MSE_10_1 = MSE(10,1,1000)
MSE_10_5 = MSE(10,5,1000)
MSE_10_50 = MSE(10,50,1000)
MSE_10_100 = MSE(10,100,1000)

# n = 30
MSE_30_1 = MSE(30,1,1000)
MSE_30_5 = MSE(30,5,1000)
MSE_30_50 = MSE(30,50,1000)
MSE_30_100 = MSE(30,100,1000)

# Align multiple graph on the same page
par(mfrow=c(3,2))

# Graphs with varying n value and fixed theta value
# Plot n = 1
plot(c(1,5,50,100), c(MSE_1_1[1], MSE_1_5[1], MSE_1_50[1], MSE_1_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 1", ylim = c(0,4000))
lines(c(1,5,50,100), c(MSE_1_1[2], MSE_1_5[2], MSE_1_50[2], MSE_1_100[2]), type = "b", col
      = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot n = 2
plot(c(1,5,50,100), c(MSE_2_1[1], MSE_2_5[1], MSE_2_50[1], MSE_2_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 2", ylim = c(0,2000))
lines(c(1,5,50,100), c(MSE_2_1[2], MSE_2_5[2], MSE_2_50[2], MSE_2_100[2]), type = "b", col
      = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot n = 3
plot(c(1,5,50,100), c(MSE_3_1[1], MSE_3_5[1], MSE_3_50[1], MSE_3_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 3", ylim = c(0,2000))
lines(c(1,5,50,100), c(MSE_3_1[2], MSE_3_5[2], MSE_3_50[2], MSE_3_100[2]), type = "b", col
      = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot n = 5
plot(c(1,5,50,100), c(MSE_5_1[1], MSE_5_5[1], MSE_5_50[1], MSE_5_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 5", ylim = c(0,1000))
lines(c(1,5,50,100), c(MSE_5_1[2], MSE_5_5[2], MSE_5_50[2], MSE_5_100[2]), type = "b", col
      = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot n = 10
plot(c(1,5,50,100), c(MSE_10_1[1], MSE_10_5[1], MSE_10_50[1], MSE_10_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 10", ylim = c(0,500))
lines(c(1,5,50,100), c(MSE_10_1[2], MSE_10_5[2], MSE_10_50[2], MSE_10_100[2]), type =
        "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot n = 30
plot(c(1,5,50,100), c(MSE_30_1[1], MSE_30_5[1], MSE_30_50[1], MSE_30_100[1]), type = "b",
     xlab = "theta", ylab = "MSE", col = 'red', main = "n = 30", ylim = c(0,200))
lines(c(1,5,50,100), c(MSE_30_1[2], MSE_30_5[2], MSE_30_50[2], MSE_30_100[2]), type =
        "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Align multiple graph on the same page
par(mfrow=c(2,2))

# Graphs with varying theta values and fixed n values.
# Plot theta = 1
plot(c(1,2,3,5,10,30), c(MSE_1_1[1], MSE_2_1[1], MSE_3_1[1], MSE_5_1[1], MSE_10_1[1],
                         MSE_30_1[1]),
     type = "b", xlab = "n", ylab = "MSE", col = 'red', main = "theta = 1", ylim = c(0,1))
lines(c(1,2,3,5,10,30), c(MSE_1_1[2], MSE_2_1[2], MSE_3_1[2], MSE_5_1[2], MSE_10_1[2],
                          MSE_30_1[2]),
      type = "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot theta = 5
plot(c(1,2,3,5,10,30), c(MSE_1_5[1], MSE_2_5[1], MSE_3_5[1], MSE_5_5[1], MSE_10_5[1],
                         MSE_30_5[1]),
     type = "b", xlab = "n", ylab = "MSE", col = 'red', main = "theta = 5", ylim = c(0,10))
lines(c(1,2,3,5,10,30), c(MSE_1_5[2], MSE_2_5[2], MSE_3_5[2], MSE_5_5[2], MSE_10_5[2],
                          MSE_30_5[2]),
      type = "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot theta = 50
plot(c(1,2,3,5,10,30), c(MSE_1_50[1], MSE_2_50[1], MSE_3_50[1], MSE_5_50[1],
                         MSE_10_50[1], MSE_30_50[1]),
     type = "b", xlab = "n", ylab = "MSE", col = 'red', main = "theta = 50", ylim = c(0,1000))
lines(c(1,2,3,5,10,30), c(MSE_1_50[2], MSE_2_50[2], MSE_3_50[2], MSE_5_50[2],
                          MSE_10_50[2], MSE_30_50[2]),
      type = "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# Plot theta = 100
plot(c(1,2,3,5,10,30), c(MSE_1_100[1], MSE_2_100[1], MSE_3_100[1], MSE_5_100[1],
                         MSE_10_100[1], MSE_30_100[1]),
     type = "b", xlab = "n", ylab = "MSE", col = 'red', main = "theta = 100", ylim = c(0,4000))
lines(c(1,2,3,5,10,30), c(MSE_1_100[2], MSE_2_100[2], MSE_3_100[2], MSE_5_100[2],
                          MSE_10_100[2], MSE_30_100[2]),
      type = "b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), cex=.8, pch=c(1,1))

# R code for question 2
# Part 2c
# Code for minimizing the function.
lkhd_function <- function(theta,x) {
  return (theta/((x)^theta))
}

neg_loglik_fun <- function(par, data) {
  result <- sum(log(lkhd_function(par,data)))
  return (-result)
}

MLEstimator <- optim(par=0.5, fn=neg_loglik_fun, method = "Nelder-Mead",
                hessian= TRUE, data= c(21.72,14.65,50.42,28.78,11.23))
print(MLEstimator$par)

# Part 2d
# R code for finding the confidence interval and the standard error
se = sqrt(1/MLEstimator$hessian)
print(se)
upperB = MLEstimator$par + qnorm(0.975) * se
lowerB = MLEstimator$par - qnorm(0.975) * se

# Upper bound
print(upperB)

# Lower bound
print(lowerB)
