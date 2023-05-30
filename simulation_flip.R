sample.space <- c(0, 1)
theta <- 0.5
N <- 1000000000000

flips <- sample(sample.space,
                size = N,
                replace = TRUE,
                prob = c(theta, 1 - theta))
sum(flips != 1) / N

count(flips == 0)
