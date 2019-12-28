### check inputs ###
## what it does: 
#   do input checking for the arguments of ransac and create
#   important variables that are required later in ransac
## output: list with 
#   @design 
#     - the design matrix for the formula where all observations with missing
#       values are dropped
#   @n_missing 
#     - how many observations contained at least one NA
#   @missing 
#     - numeric vector that contains the indices of the observations
#       (from the initial data) that do not contain NAs
#   @y 
#     - the target vector with missing values removed
#   @y_name
#     - the name of the target/y-variable
#   @n_complete_obs 
#     - number of observations that do not contain any NAs
#   @missing
#     - a logical vector with length n_obs that indicates whether the c
#   @model_loss
#     - the model_loss (is only relevant if no model_loss was specifide
#       then the model loss is set to the mean of the inlier_loss)


check_inputs <- function(formula, data, error_threshold, inlier_threshold, iterations, 
                         sample_size, inlier_loss, model_loss, seed, 
                         parallel) {
  if (!require(checkmate)) install.packages("checkmate")
  
  # we need to check that the formula is well defined, we do this with 
  # all.vars and check wether all the names exist in the colnames of data
  # the case formula = y~. has to be treated differently however
  
  formula_variables <- all.vars(formula)
  y_name <- formula_variables[1]
  
  if ("." %in% formula_variables) {
    formula_variables <- colnames(data)
  }
  
  
  checkmate::assert(check_formula(formula),
                    check_true(all(formula_variables %in% colnames(data))), 
                    combine = "and")
  
  # some preliminary checking that data is actually a data-frame
  # after removing the missings we will check again
  checkmate::assert_data_frame(data)
  
  # note that it is important that we remove those rows that have NAs iN
  # the variables that actually appear in the formula
  missing <- apply(X = data[, formula_variables], 
                   MARGIN = 1, 
                   FUN = function(x) any(is.na(x)))
  
  n_missing <- sum(missing)
  
  if (n_missing > 0) {
    warning(paste(n_missing, " observations with missing vaiables are dropped."))
  }

  
  y <- data[!missing, y_name] # if we would set drop = FALSE it would not work 
  # as y is passed to the lm function later and must not be a list 
  
  # be really careful here: model.matrix also drops the NAs but only as long as 
  # options('na.action') == 'na.omit'. so we include the !missing
  design <- model.matrix(object = formula, 
                         data = data[!missing,, drop = FALSE]) 

  n_complete_obs <- NROW(design)
  # now we check that the models fitted on the samples are actually 
  # identifiable, i.e. #beta < n. we should also check that there 
  # are more than inlier_threshold observations
  checkmate::assert(checkmate::check_matrix(design, min.cols = 1, 
                                            max.cols = sample_size - 1, 
                                            min.rows = inlier_threshold + 1))
  
  # note: there is no point in checking singularities of X^tX as we will use so many 
  # different design-matrices due do the random sub-sampling in the algorithm 
  
  # check that the error threshold is a positive number, non finite 
  checkmate::assert(checkmate::check_numeric(error_threshold, finite = TRUE, len = 1, 
                                             any.missing = FALSE), 
                    checkmate::check_true(error_threshold > 0), 
                    combine = "and")
  
  # check that the inlier_threshold is actually a count and that it is actually
  # possible their sum is lower than the number of complete observations
  checkmate::assert(checkmate::check_count(inlier_threshold), 
                    checkmate::check_true(inlier_threshold < n_complete_obs), 
                    checkmate::check_true(inlier_threshold > sample_size), 
                    combine = "and")
  
  # check that the number of repetitions is a count > 0 
  checkmate::assert_count(iterations, positive = TRUE)
  
  # check that the seed is a finite non-NA number
  checkmate::assert_numeric(seed, finite = TRUE, any.missing = FALSE, len = 1)
  
  # check that the specified loss-functions actually are functions 
  checkmate::assert(checkmate::check_function(inlier_loss), 
                    # does the function give back numeric value?
                    checkmate::check_numeric(inlier_loss(1, 2), 
                                             len = 1, any.missing = FALSE),
                    checkmate::check_numeric(inlier_loss(1, 2), 
                                             len = 1, any.missing = FALSE),
                    combine = "and")
  
  checkmate::assert(checkmate::check_function(inlier_loss),
                    checkmate::check_true(length(inlier_loss(0,1)) == 1),
                    checkmate::check_true(length(inlier_loss(c(0,1), c(1,2))) == 2), 
                    combine = "and")
  
  #
  if (is.null(model_loss)) {
    warning("model_loss was not specified and is set to the mean of the inlier_loss
            function")
    model_loss <- function(x, y) mean(inlier_loss(x,y))
  }
  
  checkmate::assert(checkmate::check_function(model_loss), 
                    checkmate::check_numeric(model_loss(0,1), 
                                             len = 1, any.missing = FALSE),
                    checkmate::check_numeric(model_loss(0:1, 1:2), 
                                             len = 1, any.missing = FALSE), 
                    combine = "and")
  
  
  
  
  # check that parallel is logical 
  checkmate::assert_logical(parallel, any.missing = FALSE, len = 1)
  
  # we give back the number of rows with missing values and the design-matrix
  list(design = design, n_missing = n_missing, y = y, y_name = y_name, 
       n_complete_obs = n_complete_obs, missing = missing, 
       model_loss = model_loss)
}

# things that are checked
# @formula: 
# - is it a formula?
# - are all the variable names that are used in formula also in the data?
# @data 
# - ist it a dataframe ? if yes we convert it to the corresponding model matrix "design"
# @design 
# - is it a matrix? 
# - does it have > inlier_threshold rows?
# - does it have more rows than columns? 
# - does it contain at least one column?
# @error_threshold 
# - is it a non-finite numeric with length 1 that is not NA?
# - is it > 0? 
# @inlier_threshold
# - is it a count? 
# - is it larger than the sample size? 
# - is it smaller than the number of complete observations? 
# @iterations
# - is it a count > 0? (that already checks whether it is NA)
# @seed
# - is it a finite numeric value with length 1 and is not an NA? 