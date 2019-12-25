### ransac_once ###
## the ransac algorithm consists of iteratively fitting a model to a random 
# 


## output: we only return the error of the model. 
# if the consensus set is to small this error is set to Inf

#TODO
# we still need to check that the result also makes sense if the fitting 
# process did not succeed or if there is multicolinearity
# in this case the coefficients are NA, which also means that 
# the predictions will be NA and the loss therefore as well 

ransac_once <- function(formula, design, inliers_maybe, y, inlier_loss, model_loss, 
                        error_threshold, n_observations, inlier_threshold) {

  # y is actually a n x 1 matrix but we can still subset 
  # like we do with a vector, as a matrix is a vector as well 
  
  ### ???  must subset_index be a logical vector or can it also be 
  # a numeric vector that contains the indices of the rows that are to be used
  
  ## here we have to take care, as the design matrix does not contain the 
  # target anymore. However this is not a problem, as long as we specify the 
  # name in the formula correctly, so maybe we should call it y again? (even 
  # though it is not the most elegant solution)
  model_initial <- lm(formula = formula, 
                      data = design, 
                      subset = inliers_maybe)
  
  coefficients_initial <- model_initial$coefficients
  
  if (any(is.na(coefficients_initial))) {
    warning("there were problems (possibly colinearity issues) with the estimation
            of a sub-model")
    return(Inf)
  }
  
  design_subset <- design[inliers_maybe, , drop = FALSE]
  y_pred_initial <- design_subset %*% coefficients_initial

  losses <- inlier_loss(y_pred = y_pred_initial, 
                        y = y[-inliers_maybe])
  # we extend the set by those that were not sampled but have a small enough loss
  inliers_also <- 1:n_observations[-inliers_maybe][losses <= error_threshold]
  
  inliers_all <- c(inliers_maybe, inliers_also)
  
  if (length(inliers_all) < inlier_threshold) {
    return(Inf)
  }
  
  
  
  model_total <- lm(formula = formula, 
                    data = design, 
                    subset = inliers_all)
  
  coefficients_total <- model_total$coefficients
  
  if (any(is.na(coefficients_initial))) {
    warning("there were problems (possibly colinearity issues) with the estimation
            of a sub-model")
    return(Inf)
  }
  
  model_loss(y = y[inliers_all],
             y_pred = design[inliers_all, , drop = FALSE] %*% coefficients_total)
}