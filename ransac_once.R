### ransac_once ###
## the ransac algorithm consists of iteratively fitting a model to a random 
# sub-sample, extending the consensus set. Fitting a model on the whole consensus
# set if it is large enough and choosing the model with the best fit on the 
# consensus set
# the funcion ransac_once does all this for one subsample. 
## output: we only return the error of the model. 
# if the consensus set is to small or something did not work (meaning there
# are NAs in the coefficient-vector)


### we dont need the formula as the design matrix is passed
ransac_once <- function(formula, design, inliers_maybe, y, inlier_loss, model_loss, 
                        error_threshold, n_observations, inlier_threshold, 
                        output) {

  # if we don't do this the models are fitted in the environment
  # we were in before we entered the ransac_once function. this is problematic
  # as we also use variables (like inliers_maybe, inliers_all) within the lm
  # call that do not exist in that environment which gives rise to a 
  # confusing error 'object inliers_maybe' not found even though it clearly
  # exists in this environment where we are now
  environment(formula) <- environment()
  
  ### estimate the initial model based on the random sample
  model_initial <- lm(formula = formula, subset = inliers_maybe)
  coefficients_initial <- model_initial$coefficients
  
  ### did the estimation succeed? 
  if (any(is.na(coefficients_initial))) {
    warning("there were problems (possibly colinearity issues) with the estimation
            of a smodel on a randomly selected sample")
    return(Inf)
  }
  
  ### we need to get the predictions for evey point that was not sampled
  # those are called potential_inliers. then we calculate losses
  design_potential_inliers <- design[-inliers_maybe, , drop = FALSE]
  y_pred_initial <- design_potential_inliers %*% coefficients_initial

  losses <- inlier_loss(y_pred_initial, y[-inliers_maybe])
  
  # we extend the set by those that were not sampled but have a small enough loss
  inliers_also <- ((1:n_observations)[-inliers_maybe])[losses <= error_threshold]
  inliers_all <- c(inliers_maybe, inliers_also)
  
  # check whether sufficiently many inliers were found
  if (length(inliers_all) < inlier_threshold) {
    return(Inf)
  }
  
  model_total <- lm(formula = formula, subset = inliers_all)
  
  coefficients_total <- model_total$coefficients
  
  if (any(is.na(coefficients_total))) {
    warning("there were problems (possibly colinearity issues) with the estimation
            of a model on a consensus set")
    return(Inf)
  }
  
  # in case everything worked with the estimation we return the loss evaluatd
  # on the whole consensus set (which has to be a scalar)
  loss_total <- model_loss(y[inliers_all],
                           design[inliers_all, , drop = FALSE] %*% coefficients_total)
  if (output == "error") return(loss_total)
  else if (output == "all_indices") {
    return(1:n_observations %in% inliers_all)
  } else {
    stop("wrongly specified output")
  }
}



