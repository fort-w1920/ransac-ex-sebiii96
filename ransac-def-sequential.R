### ransac sequential ###

sample_sequential <- sample_inliers_parallel <- function(n_observations, sample_size) {
  sample_inliers_once <- function() {
    sample(1:n_observations, size = sample_size, replace = FALSE)
  }
  # we give back a sample_size x n_observations matrix, i.e. the i-th column 
  # contains the i-th sample of indices
  future.vapply::future_replicate(x = 1L:n_observations, 
                                  FUN = sample_inliers_once, 
                                  FUN.VALUE = integer(sample_size), 
                                  future.seed = seed)
  
}

ransac_sequential <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                              sample_size, inlier_loss, model_loss = inlier_loss, seed = 314L, 
                              n_observations) {
  indices_matrix <- replicate(x = 1L:n_observations, 
                              FUN = sample)
}