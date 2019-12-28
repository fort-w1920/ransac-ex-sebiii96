source("ransac-test-data.R")
source("ransac.R")
source("ransac-utils.R")
# immer set.seed() um Ergebnisse reproduzierbar zu machen...
set.seed(1874374111)
data_simple <- make_ransac_data(n_obs = 500, n_coef = 1, inlier_fraction = 0.7)

# univariate example:
ransac_simple <- ransaclm(
  formula = y ~ . - inlier,
  data = data_simple,
  error_threshold = 1,
  inlier_threshold = 200,
  iterations = 1000,
  sample_size = 150,
  inlier_loss = loss_l2,
  model_loss = loss_l2_mean,
  seed = 2812,
  parallel = FALSE
)
validate_ransac(ransac_simple)
