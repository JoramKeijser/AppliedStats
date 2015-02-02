## 1.Earthquakes
Data <- quakes
library(MASS)
truehist(Data$mag)
x <- seq(0, 7, by = 0.1)
#estimate <- ecdf(Data)
plot(ecdf(Data$mag), main = 'Ecdf of magnitude data', xlab = 'magnitude', ylab = 'cumulative frequency')
# CI:
p_hat <- mean(Data$mag > 3.9 & Data$mag <= 4.9)
alpha <- 0.05
n = length(Data$mag)
q <- qnorm(alpha/2)
frac <- (p_hat * (1 - p_hat) + q^2/(4*n))/n
l <- (p_hat + q^2/(2*n) + q*sqrt(frac))/(1+q^2)
u <- (p_hat + q^2/(2*n) - q*sqrt(frac))/(1+q^2)

## 2.Binomimial CI
n = 10
alpha = 0.05
Covered <- function(p = 0.5) {
  X <- rbinom(n, 1, prob = p)
  phat <- mean(X)
  l <- phat - sqrt(log(2/alpha)/(2*n))
  u <- phat + sqrt(log(2/alpha)/(2*n))
 return(p >= l & p <= u)
}
B = 10^3
P = seq(0, 1, by = 0.1)
C <- data.frame(matrix(ncol = length(P), nrow = 1))
names(C) <- sapply(P, toString)
for(p in P) {
C[toString(p)] <- mean(replicate(B, Covered(p))  )
}
plot(P, C, ylim = c(0.9, 1), xlab = "p", ylab = 'coverage')
lines(c(0, 1), c(1-alpha, 1-alpha), 'l')
# Very conservative: Coverage is a lot higher. 

