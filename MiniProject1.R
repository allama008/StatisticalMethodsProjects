#Question 1)b)i)
probabDensityFunc <- function(x){return (0.2*exp(-0.1*x)-0.2*exp(-0.2*x))}
probabDensityFunc(1)

#ii)
draws10k <- replicate(10000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))

#iii
hist(x = draws10k, main = "10k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
curve(probabDensityFunc)

#iv
mean(draws10k)

#v
1 - pexp(15, rate = 1 / mean(draws10k))

#vi
#Sample Size = 10000
#First Iteration
draws10k <- replicate(10000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws10k, main = "10k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws10k)
1 - pexp(15, rate = 1 / mean(draws10k))

#Second Iteration
draws10k <- replicate(10000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws10k, main = "10k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws10k)
1 - pexp(15, rate = 1 / mean(draws10k))

#Third Iteration
draws10k <- replicate(10000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws10k, main = "10k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws10k)
1 - pexp(15, rate = 1 / mean(draws10k))

#Fourth Iteration
draws10k <- replicate(10000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws10k, main = "10k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws10k)
1 - pexp(15, rate = 1 / mean(draws10k))

#c
#Sample Size = 1000
#First Iteration, SampleSize = 1000
draws1k <- replicate(1000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws1k, main = "1k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws1k)
1 - pexp(15, rate = 1 / mean(draws1k))

#Second Iteration, SampleSize = 1000
draws1k <- replicate(1000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws1k, main = "1k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws1k)
1 - pexp(15, rate = 1 / mean(draws1k))

#Third Iteration, SampleSize = 1000
draws1k <- replicate(1000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws1k, main = "1k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws1k)
1 - pexp(15, rate = 1 / mean(draws1k))

#Fourth Iteration, SampleSize = 1000
draws1k <- replicate(1000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws1k, main = "1k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws1k)
1 - pexp(15, rate = 1 / mean(draws1k))

#Fifth Iteration, SampleSize = 1000
draws1k <- replicate(1000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws1k, main = "1k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws1k)
1 - pexp(15, rate = 1 / mean(draws1k))

#Sample Size = 100000
#First Iteration, SampleSize = 100000
draws100k <- replicate(100000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws100k, main = "100k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws100k)
1 - pexp(15, rate = 1 / mean(draws100k))

#Second Iteration, SampleSize = 100000
draws100k <- replicate(100000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws100k, main = "100k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws100k)
1 - pexp(15, rate = 1 / mean(draws100k))

#Third Iteration, SampleSize = 100000
draws100k <- replicate(100000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws100k, main = "100k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws100k)
1 - pexp(15, rate = 1 / mean(draws100k))

#Fourth Iteration, SampleSize = 100000
draws100k <- replicate(100000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws100k, main = "100k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws100k)
1 - pexp(15, rate = 1 / mean(draws100k))

#Fifth Iteration, SampleSize = 100000
draws100k <- replicate(100000, max(rexp(n = 1, rate = 1/10), rexp(n = 1, rate = 1/10)))
hist(x = draws100k, main = "100k draws from the distribution of satellite lifetime T", col = "gray", freq = FALSE)
mean(draws100k)
1 - pexp(15, rate = 1 / mean(draws100k))

#Question 2
iter <- 10000
x <- runif(iter, min = 0, max = 1)
y <- runif(iter, min = 0, max = 1)
circle <- (x - 0.5)^2 + (y - 0.5)^2 <= 0.5^2
MonteCarloPi <- (sum(circle)/iter)*4
MonteCarloPi
