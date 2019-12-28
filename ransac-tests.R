library(testthat)
source("ransac.R")
source("ransac-test-data.R")


context("testing ransac")

test_that("check_input works", {
  expect_error(check_inputs_default(formula = y ~ z)) # z not in data_simple
  expect_error(check_inputs_default(formula = "y ~."))

  expect_error(check_inputs_default(data = matrix(1:10, ncol = 2)))
  expect_error(check_inputs_default(data = list(1:10)))
  expect_error(check_inputs_default(1:10))
  expect_error(check_inputs_default(
    formula = y ~ .,
    data = data.frame(c(1:10, rep(NA, times = 100)))
  ))
  expect_warning(check_inputs_default(data = data_simple)) # give warning if there are NAs
  expect_error(check_inputs_default(formula = y ~ ., data = data_simple[1:5, ]))
  # (not enough observations without the NAs)

  expect_error(check_inputs_default(error_threshold = "hallo")) # must be numeric
  expect_error(check_inputs_default(error_threshold = as.numeric(NA))) # must not be NA
  expect_error(check_inputs_default(error_threshold = c(1, 2))) # must have length 1
  expect_error(check_inputs_default(error_threshold = Inf)) # must be finite

  expect_error(check_inputs_default(inlier_threshold = -5)) # must be count
  expect_error(check_inputs_default(inlier_threshold = 120)) # not enough observations for this threshold

  expect_error(check_inputs_default(iterations = 50.2)) # must be count
  expect_error(check_inputs_default(iterations = as.numeric(NA))) # must not be NA

  expect_error(check_inputs_default(
    sample_size = 20,
    inlier_threshold = 20
  )) # inlier_threshold must be > sample_size
  expect_error(check_inputs_default(sample_size = -5)) # must be count

  expect_error(check_inputs_default(inlier_loss = mean)) # must be vectorizable
  expect_error(check_inputs_default(inlier_loss = 1:10)) # must be a function

  expect_error(check_inputs_default(model_loss = function(x) x^2)) # must have scalar return

  expect_error(check_inputs_default(seed = "hallo")) # must be a numeric
  expect_error(check_inputs_default(seed = as.numeric(NA)))
  ### now we check that with correct inputs there is no error
  expect_error(check_inputs_default(), regexp = NA)
})

# the general idea of how we test the ransaclm function is that we first
# generate n points that lie on a hyperplane, i.e. y = X%*%beta
# without errors. then we add some points that are shifted by some amount
# upwards or downwards (for an illustration plot data_example)
# if the outlier percentage is chosen small enough or the iterations large enough
# the ransac-algorithm should be able to recover the true relationship
# y = X%*%b


test_that("ransac works", {
  expect_equivalent(
    ransaclm_default()[["model"]][["coefficients"]],
    output_simple[["model"]][["coefficients"]]
  ) # check that
  # the true parameters are being recovered
  expect_equivalent(
    ransaclm_default()[["data"]],
    output_simple[["data"]]
  ) # check that the correct data-set
  # is given back
  expect_equivalent(
    ransaclm_default(parallel = TRUE), # check that parallelization works
    ransaclm_default(parallel = FALSE)
  )
  expect_equivalent(
    ransaclm_default(model_loss = NULL), # check that model_loss is actually set
    # to the mean of inlier_loss if left unspecified
    ransaclm_default(model_loss = loss_l2_mean)
  )
})
