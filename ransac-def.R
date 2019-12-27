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
  set.seed(seed)
  
  ### parallelization ###
  # install packages if not already installed
  if (!require("future")) install.packages("future")
  if (!require("future.apply")) install.packages("future.apply")

  plan_before <- future::plan()
  on.exit(future::plan(plan_before))
  
  if (parallel) {
    future::plan("multiprocess")
  } else {
    future::plan("sequential")
  }
  
  ### input checkign and creation of relevant variables ###
  checked_input <- check_inputs(formula = formula, data = data, seed = seed,
                                error_threshold = error_threshold, 
                                inlier_threshold = inlier_threshold, 
                                iterations = iterations, sample_size = sample_size, 
                                inlier_loss = inlier_loss, model_loss = model_loss,  
                                parallel = parallel) 
  
  # this should be called design_matrix to make it clear it is no data frame
  design <- checked_input$design
  y <- checked_input$y 
  n_complete_obs <- checked_input$n_complete_obs
  missing <- checked_input$missing #this is needed later to get the used_data 
  model_loss <- checked_input$model_loss #in case model_loss is left unspecified
  # it is defined to be the mean of the inlier_loss
  n_obs <- NROW(data) 
  
  ### we create a formula that uses the model_matrix: "y ~ mm - 1"
  # we have to include -1, as the model matrix already includes the intercept
  # column if specified by the user
  target_name <- all.vars(formula)[1]
  formula <- as.formula(paste(target_name, " ~ design - 1", sep = ""))
  
  ### generate random_samples ###
  index_matrix <- future.apply::future_replicate(n = iterations, 
                                                 sample(x = 1:n_complete_obs, 
                                                        size = sample_size)) 
  
  ### ransac-algorithm ###
  
  # errors <- ransac_once(design = design, 
  #                       formula = formula, 
  #                       y = y, 
  #                       inlier_loss = inlier_loss, model_loss = model_loss, 
  #                       error_threshold = error_threshold, 
  #                       n_observations = n_complete_obs, 
  #                       inlier_threshold = inlier_threshold, 
  #                       inliers_maybe = index_matrix[,1])
  # 
  
  # preallocation of memory space
  # comment: ransac_once only gives back the errors and we can recover the 
  # best model by that error. this safes memory space although we have 
  # to reestimate the optimal model -> what is better ???
  errors <- vector(length = iterations)
  errors <- future.apply::future_apply(X = index_matrix,
                                      FUN = ransac_once,
                                      MARGIN = 2, # additional arguments
                                      formula = formula,
                                      design = design,
                                      y = y,
                                      inlier_loss = inlier_loss,
                                      model_loss = model_loss,
                                      error_threshold = error_threshold,
                                      n_observations = n_complete_obs,
                                      inlier_threshold = inlier_threshold, 
                                      output = "error")
  
  if (all(errors == Inf)) {
    warning("ransac-algorithm did not succeed.")
    return(NULL)
  }
  
  ### get optimal model(s) ###
  best_model_index <- which(errors == min(errors))
  if (length(best_model_index) > 1) {
    warning("there are at least two models with equal performance. The optimal 
            model is randomly selected.")
    best_model_index <- sample(x = best_model_index, size = 1)
  }
  
  # get the points which were used for the optimal model
  optimal_points <- ransac_once(formula = formula,
                                inliers_maybe = index_matrix[,best_model_index],
                                design = design,
                                y = y,
                                inlier_loss = inlier_loss,
                                model_loss = model_loss,
                                error_threshold = error_threshold,
                                n_observations = n_complete_obs,
                                inlier_threshold = inlier_threshold, 
                                output = "all_indices")
  # estimtate the optimal model
  optimal_model <- lm(formula = formula, subset = optimal_points)
  
  # generate the data that was used for the estimation of the optimal model
  used_data <- data.frame(data[!missing, , drop   = FALSE], 
                          "logical" = optimal_points)
  
  # because we estimated the model with y ~ design - 1 the names of the coeficients
  # are design(Intercept), therefore we have to remove the "design" of the names
  # as a variable could also be called "design" we do this by removing the first
  # 6 letters

  names(optimal_model[["coefficients"]]) <- colnames(design)
  
  list("model" = optimal_model, 
       "data" = used_data)
}


