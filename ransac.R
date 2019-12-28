source(here::here("ransac-input-checking.R"))
source(here::here("ransac-once.R"))

### ransaclm ###
## inputs ##
# @formula
#   is the formula that is to be fitted by lm
# @data
#   must be a dataframe that contains all the variables specified in formula
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
#   size of the random sample on the basis of which the models are initially estimated
# @inlier_loss
#   which metric do we use to assess whether points have an
#   acceptable error. MUST BE VECTORIZABLE
# @model_loss
#   how do we assess the quality of the whole model -
#   is a SCALER LOSS FUNCTION
#   If NULL (the default) it is set to the mean of the inlier_loss
# @seed
#   which seed should be set before sampling
# @parallel
#   logical variable that indicates whether the algorithm should be run in parallel

## output ##
# a list that contains:
#   "model" - the fitted optimal model
#   "data"  - the data that was used, i.e. the input data without the
#             rows that had missing values in relevant columns
#             and extended by a row ".consensus_set" that indicates whether the
#             corresponding observation was contained in the best consensus set

ransaclm <- function(formula, data, error_threshold, inlier_threshold, iterations,
                     sample_size, inlier_loss, model_loss = NULL, seed = 314L,
                     parallel = FALSE) {
  set.seed(seed)
  if (!require("future")) install.packages("future")
  if (!require("future.apply")) install.packages("future.apply")

  ### input checking and creation of relevant variables ###
  checked_input <- check_inputs(
    formula = formula, data = data, seed = seed,
    error_threshold = error_threshold,
    inlier_threshold = inlier_threshold,
    iterations = iterations, sample_size = sample_size,
    inlier_loss = inlier_loss, model_loss = model_loss,
    parallel = parallel
  )

  design <- checked_input[["design"]]
  y <- checked_input[["y"]]
  y_name <- checked_input[["y_name"]]
  missing <- checked_input[["missing"]]
  n_complete_obs <- NROW(design)
  # in case model_loss == NULL check_inputs set model_loss to be the mean
  # of the inlier _loss
  if (is.null(model_loss)) model_loss <- checked_input[["model_loss"]]




  ### we create a formula that uses the design-matrix: "y ~ design - 1"
  # -1 has to be included as the design-matrix already contains the intercept
  # the reason why the estimation is based on the design-matrix is that therefore
  # the design-matrix does not have to be created in each iteration of the
  # ransac-algorithm
  design_formula <- as.formula(paste(y_name, " ~ design - 1", sep = ""))


  ### possible parallelization ###
  plan_before <- future::plan()
  on.exit(future::plan(plan_before))

  if (parallel) {
    future::plan("multiprocess")
  } else {
    future::plan("sequential")
  }


  ### generate random_samples ###
  index_matrix <- future.apply::future_replicate(
    n = iterations,
    sample(
      x = 1:n_complete_obs,
      size = sample_size
    )
  )

  # fit all the models and give back the corresponding quality of fit of the models
  # which are saved in the vector 'errors'

  errors <- vector(length = iterations)
  errors <- future.apply::future_apply(
    X = index_matrix,
    FUN = ransaclm_once,
    MARGIN = 2,
    formula = design_formula,
    design = design,
    y = y,
    inlier_loss = inlier_loss,
    model_loss = model_loss,
    error_threshold = error_threshold,
    n_observations = n_complete_obs,
    inlier_threshold = inlier_threshold,
    output = "error"
  )

  # the error is set to Inf if either the inlier_threshold is not surpassed
  # or the estimation did procude NAs
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
  # by the ransaclm_once function, but we have to speficy output = "model_and_indices"
  # as opposed to output = "error" like before

  model_and_indices <- ransaclm_once(
    formula = design_formula,
    inliers_maybe = index_matrix[, best_model_index],
    design = design,
    y = y,
    inlier_loss = inlier_loss,
    model_loss = model_loss,
    error_threshold = error_threshold,
    n_observations = n_complete_obs,
    inlier_threshold = inlier_threshold,
    output = "model_and_indices"
  )

  optimal_model <- model_and_indices[["model"]]


  output_data <- data.frame(data[!missing, , drop = FALSE],
    ".consensus_set" = model_and_indices[["indices"]]
  )

  # because we estimated the model with y ~ design - 1 the names of the coeficients
  # are design(Intercept), design(X1), ... which we don't want

  names(optimal_model[["coefficients"]]) <- colnames(design)

  list(
    "model" = optimal_model,
    "data" = output_data
  )
}
