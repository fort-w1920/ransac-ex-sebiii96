require(here)

source(here::here("input_checking.R"))
source(here::here("ransac_once.R"))
source(here::here("ransac-utils.R"))

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
  ### parallelization ###
  require(future)
  require(future.apply)
  
  plan_before <- plan()
  on.exit(plan(plan_before))
  
  if (parallel) {
    plan("parallel")
  } else {
    plan("sequential")
  }
  
  ### input checkign and creation of relevant variables ###
  checked_input <- check_inputs(formula = formula, data = data, seed = seed,
                                error_threshold = error_threshold, 
                                inlier_threshold = inlier_threshold, 
                                iterations = iterations, sample_size = sample_size, 
                                inlier_loss = inlier_loss, model_loss = model_loss,  
                                parallel = parallel) 
  
  design <- checked_input$design
  n_mising <- checked_input$n_missing
  y <- checked_input$y
  n_complete_obs <- checked_input$n_complete_obs
  missing <- checked_input$missing
  model_loss <- checked_input$model_loss #in case model_loss is left unspecified
  # it is defined to be the mean of the inlier_loss
  n_obs <- NROW(data) 
  
  ### generate random_samples ###
  index_matrix <- future.apply::future_replicate(n = iterations, 
                                                 sample(x = 1:n_complete_obs, 
                                                        size = sample_size)) 
  
  ### ransac-algorithm ###
  
  errors <- ransac_once(design = design, 
                        formula = formula, 
                        y = y, 
                        inlier_loss = inlier_loss, model_loss = model_loss, 
                        error_threshold = error_threshold, 
                        n_observations = n_complete_obs, 
                        inlier_threshold = inlier_threshold, 
                        inliers_maybe = index_matrix[,1]
                        )
  
  # errors <- future.apply::future_vapply(X = index_matrix, 
  #                                       FUN = ransac_once, 
  #                                       FUN.VALUE = numeric(1), # additional arguments
  #                                       formula = formula, 
  #                                       design = design, 
  #                                       y = y, 
  #                                       inlier_loss = inlier_loss, 
  #                                       model_loss = model_loss, 
  #                                       error_threshold = error_threshold, 
  #                                       n_observations = n_complete_obs, 
  #                                       inlier_threshold = inlier_threshold)
  
  if (all(errors == Inf)) {
    warning("ransac-algorithm did not succeed.")
    return(NULL)
  }
  
  ### get optimal model(s) ###
  best_model_indices <- which(errors == min(errors))
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

ransac_data <- make_ransac_data(n_obs = 1000, n_coef = 10, inlier_fraction = 0.7)
ransac_data <- ransac_data[,-12]
inlier_loss1 <- function(x,y) (x - y)^2

ransac(formula = formula(y~. ), data = ransac_data, error_threshold = 20, 
       inlier_threshold = 100, iterations = 100, sample_size = 70,
       inlier_loss = inlier_loss1)

