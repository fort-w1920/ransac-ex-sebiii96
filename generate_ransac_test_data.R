### generate data for the testing of the ransac function ###

n <- 10000
a <- 1 
b <- 2
sd_y <- 1
sd_x <- 10
x <- rnorm(n, sd = sd_x)
y <- a + b * x + rnorm(n, sd = sd)

dat <- data.frame(y = y, x = x)
plot(dat)

l2_loss <- function(x, y) (x - y)^2
MSE <- function(x, y) mean((x - y)^2)