# here we create the data that is used for testing the ransaclm function
# in addition to that we create functions that call ransaclm and check_inputs
# but with more default values, so that the code
# in the ransac-test-file is better readable



source("ransac.R")

set.seed(314)

### specify loss functions ###
# loss_l2 will be used for inlier_loss
loss_l2 <- function(x, y) (x - y)^2
# loss_l2_mean will be used for model_loss
loss_l2_mean <- function(x, y) mean((x - y)^2)

### generate the function that creates test-data ###
# see data_example above how this looks in the univariate case

make_test_data <- function(n_inlier, n_outlier, n_NA, n_coef, intercept,
                           inlier_loss, model_loss, sd_x, coef_x = 1:n_coef,
                           outlier_shift, seed) {
  set.seed(seed)
  n <- n_inlier + n_outlier
  x <- matrix(rnorm(
    n = n * n_coef,
    sd = sd_x
  ), nrow = n, ncol = n_coef)
  y <- c(intercept +
    x[1:n_inlier, , drop = FALSE] %*%
    coef_x, intercept + outlier_shift + x[(n_inlier + 1):n, , drop = FALSE] %*% coef_x)
  x <- data.frame(x)
  data <- data.frame(cbind(y, x))
  x_NA <- data.frame(matrix(NA, nrow = n_NA, ncol = n_coef))
  data_NA <- data.frame(rnorm(n_NA), x_NA)
  colnames(data_NA) <- colnames(data)

  output <- rbind(data, data_NA)
}

# example how the test-data looks in the univariate case
data_example <- make_test_data(
  n_inlier = 100, n_outlier = 10, n_NA = 10, n_coef = 1,
  intercept = 3, inlier_loss = loss_l2,
  model_loss = loss_l2_mean, sd_x = 10,
  outlier_shift = 10, seed = 314
)

plot(data_example[["x"]], data_example[["y"]], main = "examplaric data", 
     xlab = "x", ylab = "y", pch = 16)

# the data that will actually be used when testing the ransac function
data_simple <- make_test_data(
  n_inlier = 100, n_outlier = 10, n_NA = 10, n_coef = 5,
  intercept = 3, inlier_loss = loss_l2,
  model_loss = loss_l2_mean, sd_x = 10,
  outlier_shift = 10, seed = 314
)


# the correct output against which the output of ransaclm will be compared
model_simple <- lm(y ~ ., data = data_simple[1:100, ])
output_data_simple <- data.frame(data_simple[1:110, ],
  ".consensus_set" = c(
    rep(TRUE, times = 100),
    rep(FALSE, times = 10)
  )
)

output_simple <- list(
  "model" = model_simple,
  "data" = output_data_simple
)


# these functions are equivalent to the ransaclm and check_input funnctions
# but with more default arguments (the ones specified above) to make the testing
# code better readable
ransaclm_default <- function(formula = y ~ .,
                             data = data_simple,
                             error_threshold = 5,
                             inlier_threshold = 50,
                             iterations = 100,
                             sample_size = 10,
                             inlier_loss = loss_l2,
                             model_loss = loss_l2_mean,
                             seed = 314,
                             parallel = FALSE) {
  # otherwise the default does not work as the formula is specified here
  # we want the environment of the formula to be the environment from which
  # the function is called
  environment(formula) <- parent.frame()
  ransaclm(formula, data, error_threshold, inlier_threshold, iterations,
    sample_size, inlier_loss,
    model_loss = NULL, seed = seed,
    parallel = parallel
  )
}

check_inputs_default <- function(formula = y ~ .,
                                 data = data_simple,
                                 error_threshold = 5,
                                 inlier_threshold = 50,
                                 iterations = 100,
                                 sample_size = 10,
                                 inlier_loss = loss_l2,
                                 model_loss = loss_l2_mean,
                                 seed = 314,
                                 parallel = FALSE) {
  # otherwise the default does not work as the formula is specified here
  # we want the environment of the formula to be the environment from which
  # the function is called
  environment(formula) <- parent.frame()
  check_inputs(
    formula, data, error_threshold, inlier_threshold, iterations,
    sample_size, inlier_loss, model_loss, seed, parallel
  )
}
