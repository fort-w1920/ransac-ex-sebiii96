### ransac-test-data ###

source(here::here("ransac-utils.R"))
source(here::here("input-checking.R"))
source(here::here("ransac-def.R"))

set.seed(314)

loss_l2 <- function(x, y) (x - y)^2
loss_l2_mean <- function(x, y) mean((x - y)^2)

make_test_data <- function(n_inlier, n_outlier, n_NA, n_coef, intercept, 
                           inlier_loss, model_loss, sd_x, coef_x = 1:n_coef, 
                           outlier_shift, seed) {
  set.seed(seed)
  n <- n_inlier + n_outlier
  x <- t(replicate(n = n, 
                   expr = rnorm(n_coef, sd_x)))
  y <- c(intercept + x[1:n_inlier, ] %*% coef_x, intercept + outlier_shift + x[(n_inlier + 1):n,] %*% coef_x)
  x <- data.frame(x)
  data <- data.frame(cbind(y, x))
  x_NA <- data.frame(matrix(NA, nrow = n_NA, ncol = n_coef))
  data_NA <- data.frame(y = rnorm(n_NA), x_NA)
  
  output <- rbind(data, data_NA)
}

data_simple <- make_test_data(n_inlier = 100, n_outlier = 10, n_NA = 10, n_coef = 5,
                              intercept = 3, inlier_loss = loss_l2, 
                              model_loss = loss_l2_mean, sd_x = 10,
                              outlier_shift = 10, seed = 314)



check_output_simple <- list(design = model.matrix(y~., data_simple), n_missing = 10, 
                            y = data_simple[1:110,"y"], n_complete_obs = 100, 
                            missing = c(rep(FALSE, times = 110), rep(TRUE, times = 10)), 
                            model_loss = loss_l2_mean)
model_simple <- lm(y ~., data = data_simple[1:100, ])
output_data_simple <- data.frame(data_simple[1:110,], logical = c(rep(TRUE, times = 100), 
                                                           rep(FALSE, times = 10)))

output_simple <- list("model" = model_simple, 
                      "data" = output_data_simple)


# this function has all the default specified which makes the testing code
# more readable 
ransac_default <- function(formula = y ~., 
                           data = data_simple, 
                           error_threshold = 5, 
                           inlier_threshold = 50, 
                           iterations = 100, 
                           sample_size = 10, 
                           inlier_loss = loss_l2, 
                           model_loss = loss_l2_mean, 
                           seed = 314, 
                           parallel = FALSE) {
  environment(formula) <- parent.frame()
  ransac(formula, data, error_threshold, inlier_threshold, iterations, 
          sample_size, inlier_loss, model_loss = NULL, seed = seed, 
          parallel = parallel)
  
}

# this function sets all the defaults except for formula (defaulting this
# here can yield to problems to due the specified environment)
check_inputs_default <- function(formula = y~., 
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
  check_inputs(formula, data, error_threshold, inlier_threshold, iterations, 
                sample_size, inlier_loss, model_loss, seed, parallel)
}

