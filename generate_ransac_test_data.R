### generate data for the testing of the ransac function ###

n <- 1000

generate_data <- function(n, n_param) {
  
}

x <- rnorm(1000)
y <- 2*x + rnorm(1000, 0.3)
test_data <- data.frame(y = y, x = x)
l2_inlier_loss <- function(x, y) (x - y)^2
l2_model_loss <- function(x, y) mean((x - y)^2)
n_obs <- 00