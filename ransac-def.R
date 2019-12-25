require(here)

source(here::here("input_checking.R"))
source(here::here("ransac_once.R"))

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
#   acceptable distance to the fitted model: is a function f(x,y) that
#   is VECTORIZABLE
#   
# @model_loss - how do we assess the quality of the whole model - 
#   is a scalar loss function. If NULL (the default) it is set
#   to the mean of the inlier_loss





## question: how do I minimize overhead ??? 
# e.g. is it ok to open multiprocess, exit and open again ??? probably not 


ransac <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                   sample_size, inlier_loss, model_loss = NULL, seed = 314L, 
                   parallel = FALSE)  {
  require(future)
  require(future.apply)
  
  # PSEUDOCODE 
  plan_before <- getplan()
  on.exit(plan(plan_before))
  
  # do input-checking and create the design-matrix, n_missings and the 
  # corsponding y-matrix/vector
  checked_input <- check_inputs(formula, data, error_threshold, inlier_threshold, iterations, 
                                sample_size, inlier_loss, model_loss = inlier_loss, seed = 314L, 
                                paralell = FALSE) 
  
  design <- checked_input$design
  n_mising <- checked_input$n_missing
  y <- checked_input$y
  n_complete_obs <- checked_input$n_complete_obs
  missing <- checked_input$missing
  n_obs <- NROW(data) # we need this later, when we transfer the indices
  # from the design matrix to the original data-set (in case of missing values)
  
  if (parallel) {
    plan("parallel")
  } else {
    plan("sequential")
  }
  
  # here we sample the index_matrix 
  index_matrix <- future.apply::future_replicate(n = iterations, 
                                                 sample(x = 1:n_complete_obs, 
                                                        size = sample_size)) 
  # here we lapply the ransac_once function
  

  # trade-off between readability of code and the choice of a pure function
  # it should also work without passing the additional arguments 
  errors <- future.apply::future_vapply(X = index_matrix, 
                                        FUN = ransac_once(inliers_maybe), 
                                        FUN.VALUE = numeric(1), # additional arguments
                                        formula = formula, 
                                        design = design, 
                                        y = y, 
                                        inlier_loss = inlier_loss, 
                                        model_loss = model_loss, 
                                        error_threshold = error_threshold, 
                                        n_observations = n_complete_obs, 
                                        inlier_threshold = inlier_threshold)
  
  if (all(errors == Inf)) {
    warning("ransac-algorithm did not succeed.")
    return(NULL)
  }
  
  # get index of best fit
  best_model_indices <- which(errors == max(errors))
  
  if (length(best_model_indices) > 1) {
    warning("there are at least two models with equal performance. The optimal 
            model is randomly selected.")
  }
  
  best_model_index <- sample(x = best_model_indices, size = 1)
  
  optimal_points <- index_matrix[, best_model_index]
  
  optimal_model <- lm(formula = formula, 
                      data = design, 
                      subset = optimal_points)
  
  optimal_points_logical <- 1:n_complete_obs %in% optimal_points
  
  used_data <- data.frame(data[!missing, , drop   = FALSE], 
                          "logical" = optimal_points_logical)
  
  list("model" = optimal_model, 
       "data" = used_data)
  
}


ransac(formula = formula(y~.), data = ransac_data, error_threshold = 20, 
       inlier_threshold = 100, iterations = 100, sample_size = 70,
       inlier_loss = function(y))
