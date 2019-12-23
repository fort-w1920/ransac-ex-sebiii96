### ransac ###
## inputs
# @data - is a data-frame that contains the data
# @estimate_model - is a function that 

#' Pseudocode
#' Initialize error <- Inf
#' Repeat iterations times 
#' Initialize maybe_inliers <- Sample sample_size points randomly
#' Fit linear model with formula: maybe_model <- lm(formula, data, subset = maybe_inliers)
#' 
#' Initialize a vector (inLier) that has length n_data - sample_size
#' for all points that are not in the random sample
#' add them to inLier if the loss (y_pred, y) is smaller than inlier_threshold
#' fit model to maybe_inliers & inliers
#' 


## question: how do I minimize overhead ??? 
# e.g. is it ok to open multiprocess, exit and open again ??? probably not 

## question: do we need more than one seed, are there other stochastic elements
# execpt for the sampling (?) I guess not

ransac <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                   sample_size, inlier_loss, model_loss = inlier_loss, seed = 314L) {
  ### begin input checking ###
  

  
  require(future.apply)
  require(future)
  
  used_variables <- all.vars(formula)
  y_name <- used_variables[1]
  
  design <- model.matrix(formula = formula, 
                           object = data)
  y <- data[[y_name]]
  
  # in case there are a lot of unnecessary 
  data_reduced <- data[,used_variables]
  
  n_observations <- NCOL(data)
  all_indices <- 1:n_onservations
  max_also_inliers <- n_observations - sample_size
  ### first we draw all the indices for the samples 
  
  ### attention ### we need to parallelize the seeding as well, this is done with 
  # future_replicate but still check how the seeds are generated, can we store 
  # them somehow and give them back ? would be nice 
  
  maybe_inliers <- sample_inliers(n_observations = n_observations, 
                                  sample_size = sample_size)
  
  # now we parallelize the actual ransac-function
  
  
  ###
  
}
  
sample_inliers <- function(n_observations, sample_size) {
  plan(multiprocess)
  on.exit(plan(sequential))
  
  sample_inliers_once <- function() {
    sample(1:n_observations, size = sample_size)
  }
  # we give back a sample_size x n_observations matrix, i.e. the i-th column 
  # contains the i-th sample of indices
  future.vapply::future_replicate(x = 1L:n_observations, 
                                  FUN = sample_inliers_once, 
                                  FUN.VALUE = integer(sample_size), 
                                  future.seed = seed)
  
}


### lm_ransac ###
# this is a function that fits a linear model 

# there are different possibilities: I can either let lm_ransac do the whole
# job or i 

# in the lm_ransac we can modlarize a bit more, i.e. the fitting of the model 
# and the calculation of the loss 

lm_ransac <- function(data, indices, formula) {
  model <- lm(formula = formula, 
              data = data, 
              subset = indices)
  coefficients <- model$coefficients # can be treated as a column-vector

  # now we vectorize the loss
  
  
  # loss function must be vectorizable 
  loss <- loss(predicted_values = design[index,] %*% coefficients, 
               actual_values = y[indices])
  
  # we first susbet the indices that are not already in the maybe_inliers
  # set and afterwards we subset those cases that 
  also_inliers <- all_indices[-indices][loss <= error_threshold]
  
  final_model <- lm(formula = formula, 
                    data = data, 
                    subset = c(indices, also_inliers))
  
  final_coefficients <- final_model$coefficients
  
  final_loss <- 
  
}

eval_lm <- function(data, formula, indices) {
  coefficients <- lm(formula = formula, 
                     data = data, 
                     subset = indices)$coefficients
  
  loss <- 
}
  