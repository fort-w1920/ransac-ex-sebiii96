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
                        error_threshold, n_observations, inlier_threshold) {

  ### we create a formula that uses the model_matrix: "y ~ mm - 1"
  # we have to include -1, as the model matrix already includes the intercept
  # column if specified by the user
  target_name <- all.vars(formula)[1]
  formula <- as.formula(paste(target_name, " ~ design - 1", sep = ""))
  
  ### estmate the model based on the random sample
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
  inliers_also <- (1:n_observations[-inliers_maybe])[losses <= error_threshold]
  inliers_all <- c(inliers_maybe, inliers_also)
  
  # check whether sufficiently many 
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
  loss_total
}

ransac_data <- make_ransac_data(n_obs = 1000, n_coef = 10, inlier_fraction = 0.7)
ransac_data <- as.data.frame(ransac_data[,-12])
design1 <- model.matrix(y~., ransac_data)
y = ransac_data[,"y"]
inlier_loss1 <- function(x,y) (x - y)^2
model_loss1 <- function(x, y) mean((x - y)^2)
error_threshold1 = 10
n_observations1 = 1000
inlier_threshold1 = 200

ransac_once(formula = formula(y  ~. -1), 
            design = design1,
            inliers_maybe = inliers_maybe1, 
            y = y, 
            inlier_loss = inlier_loss1, 
            model_loss = model_loss1, 
            error_threshold = error_threshold1, 
            n_observations = 1000,
            inlier_threshold = inlier_threshold1)




