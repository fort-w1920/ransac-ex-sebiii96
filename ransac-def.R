require(here)

source(here::here("input_checking.R"))

### ransac ###
## inputs
# @formula - is a formula that is used to fit the linear models
# @data - is a data-frame that contains all the relevant data 
# @error_threshold - is a positive numeric-value that gives the threshold 
#   when a point that is not used in the fitting process is also 'accepted' as
#   a inlier
# @inlier_threshold - is a count > 0 that indicates how many points - that are 
#   not used in the fitting proces - must have a loss smaller than error_threshold
#   such that the model is actually considered a candidate
# @iterations - the number of sub-samples (or sub-models) that are to be fitted
# @sample_size - size of the random sample that is used for the fitting of
#   the sub-model 
# @inlier_loss - which metric do we use to assess whether points have an 
#   acceptable distance to the fitted model: is a function f(y, y_pred) that
#   is VECTORIZABLE
#   ARGUMENTS MUST ALSO BE CALLED LIKE THAT
# @model_loss - how do we assess the quality of the whole model - 
#   should in the most cases be identical to the inlier_loss and must also have 
#   the form f(y, y_pred) and must also be VECTORIZABLE 




## question: how do I minimize overhead ??? 
# e.g. is it ok to open multiprocess, exit and open again ??? probably not 

## question: do we need more than one seed, are there other stochastic elements
# execpt for the sampling (?) I guess not

# we distinguish two cases: parellelized computation and non-parallelized computation


ransac <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                   sample_size, inlier_loss, model_loss = inlier_loss, seed = 314L, 
                   paralell = FALSE)  {
  
  # do input-checking and create the design-matrix, n_missings and the 
  # corsponding y-matrix/vector
  checked_input <- check_inputs(formula, data, error_threshold, inlier_threshold, iterations, 
                                sample_size, inlier_loss, model_loss = inlier_loss, seed = 314L, 
                                paralell = FALSE) 
  
  design <- checked_input$design
  n_mising <- checked_input$n_missing
  y <- checked_input$y
  
  # the actual calculation: 
  if (paralell) {
    output <- ransac_parallel(formula, design, error_threshold, inlier_threshold, iterations, 
                              sample_size, inlier_loss, model_loss, seed = 314L, 
                              paralell = FALSE)
    return(output)
  }
  
  output <- ransac_sequential(formula, design, error_threshold, inlier_threshold, iterations, 
                              sample_size, inlier_loss, model_loss, seed = 314L, 
                              paralell = FALSE)
  
  output
}

ransac_paralell <- function(formula, data, error_threshold, inlier_threshold, iterations, 
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
  all_indices <- 1:n_observations
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
  
.sample_inliers <- function(n_observations, sample_size) {
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

}
  