require(here)

source(here::here("input-checking.R"))
source(here::here("ransac-once.R"))
source(here::here("ransac-utils.R"))

### ransac ###
## inputs ##
# @formula
#   is a formula that is used to fit the linear models
# @data
#   is a data-frame that contains all the relevant data 
# @error_threshold
#   is a positive numeric-value that gives the threshold 
#   when a point that is not used in the fitting process is also 'accepted' as
#   a inlier
# @inlier_threshold
#   is a count > 0 that indicates how many points - that are 
#   not used in the fitting proces - must have a loss smaller than error_threshold
#   such that the model is actually considered a candidate
# @iterations 
#   the number of iterations for the ransac-algorithm
# @sample_size
#   size of the random sample that is used for the fitting of
#   each subsample
# @inlier_loss
#   which metric do we use to assess whether points have an 
#   acceptable distance to the fitted model: is a function f(x,y) that
#   is VECTORIZABLE
# @model_loss 
#   how do we assess the quality of the whole model - 
#   is a scalar loss function. If NULL (the default) it is set
#   to the mean of the inlier_loss
# @seed
#   which seed should be set before sampling the various subsamples
# @parallel 
#   should the algorithm be run in parallel?

## output ##
# a list that contains: 
#   "model" - the fitted optimal model 
#   "data" - the data that was used, i.e. the input data without the 
#            rows that had missing values in relevant columns
#            and extended by a row "logical" that indicates whether the corresponding
#            observation was contained in the best consensus set

## some comments ##
# - for the non-parallel execution it is probably more memory-efficient 
# to simply write a sequential loop and update the optimal model and the error
# in each iteration, but that would make the code more complicated
# 
# - the optimal model gets estimated three times which is of course unnece


ransac <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                   sample_size, inlier_loss, model_loss = NULL, seed = 314L, 
                   parallel = FALSE)  {
  set.seed(seed)
  if (!require("future")) install.packages("future")
  if (!require("future.apply")) install.packages("future.apply")

  ### input checkign and creation of relevant variables ###
  checked_input <- check_inputs(formula = formula, data = data, seed = seed,
                                error_threshold = error_threshold, 
                                inlier_threshold = inlier_threshold, 
                                iterations = iterations, sample_size = sample_size, 
                                inlier_loss = inlier_loss, model_loss = model_loss,  
                                parallel = parallel) 
  
  design <- checked_input$design
  y <- checked_input$y 
  y_name <- checked_input$y_name
  missing <- checked_input$missing 
  n_complete_obs <- NROW(design)
  # in case model_loss == NULL the model_loss is set to the mean of the inlier_loss
  model_loss <- checked_input$model_loss 


  
  
  ### we create a formula that uses the design-matrix: "y ~ design - 1"
  # -1 has to be included as the design-matrix already contains
  design_formula <- as.formula(paste(y_name, " ~ design - 1", sep = ""))
  
  
  ### set the plan according to the action of 'choice' ###
  plan_before <- future::plan()
  on.exit(future::plan(plan_before))
  
  if (parallel) {
    future::plan("multiprocess")
  } else {
    future::plan("sequential")
  }
  
  ### generate random_samples ###
  index_matrix <- future.apply::future_replicate(n = iterations, 
                                                 sample(x = 1:n_complete_obs, 
                                                        size = sample_size)) 
  
  ### ransac-algorithm ###
  
  # preallocation of memory space
  # comment: ransac_once only gives back the errors and we can recover the 
  # column from index_matrix (i.e. the subsample) that yielded the best fit
  # with that information and can reestimate the model 

  errors <- vector(length = iterations)
  errors <- future.apply::future_apply(X = index_matrix,
                                      FUN = ransac_once,
                                      MARGIN = 2, 
                                      formula = design_formula,
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
  
  # get the best model and the corresponding consensus set: this can also be done
  # by the ransac_once function, but we have to speficy output = "model_and_indices"
  
  model_and_points <- ransac_once(formula = design_formula,
                                  inliers_maybe = index_matrix[,best_model_index],
                                  design = design,
                                  y = y,
                                  inlier_loss = inlier_loss,
                                  model_loss = model_loss,
                                  error_threshold = error_threshold,
                                  n_observations = n_complete_obs,
                                  inlier_threshold = inlier_threshold, 
                                  output = "model_and_indices")
  
  optimal_model <- model_and_points[["model"]]
  
   
  output_data <- data.frame(data[!missing, , drop   = FALSE], 
                          "logical" = model_and_points[["indices"]])
  
  # because we estimated the model with y ~ design - 1 the names of the coeficients
  # are design(Intercept), design(X1), ... which we don't want

  names(optimal_model[["coefficients"]]) <- colnames(design)
  
  list("model" = optimal_model, 
       "data" = output_data)
}


