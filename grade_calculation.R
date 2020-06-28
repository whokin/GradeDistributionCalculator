library(ggplot2)

rgbeta <- function(n, mean, sd, min = 0, max = 50)
{
  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- sd^2 / (max - min)^2
  
  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)
  
  # generate standard beta observations and transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(y)
}

# Insert grade percentage in that order
# You can find an approximation of your course at http://coursediggers.com/

# A range (A+, A, A-)
a_range = c(5.6, 9.23, 12.34)
# B range (B+, B, B-)
b_range = c(16.29, 20.43, 12.1)
# C range (C+, C, C-)
c_range = c(10.73, 7.39, 4.63)
# D
d_range = 0.81
# F
f_range = 0.43

# Convert into probability
grades = c(a_range, b_range, c_range, d_range, f_range)/100

# set.seed(42)

# Number of students in the class
n <- 158
y <- rgbeta(n, mean = 39.07, sd = 5.62, min = 10, max = 50) # need to add bias to counter balance the tail of the distribution

# Summary check
sapply(list(mean, sd, min, max, median), function(f) f(y))

grade_bins = c()

for(i in cumsum(grades)){
  p = quantile(y, 1-i)
  grade_bins = c(grade_bins, p)
}

hist(y, xlim = c(0,50), breaks = seq(0,50,5))

sapply(list(mean, sd, min, max, median), function(f) f(y))


