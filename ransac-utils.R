
# generate toy data with very clear inlier / outlier distinction to
# validate ransaclm. intercept is 0, coefs have value 1.
# inputs: as named -- number  of observations, number of coefs, fraction of inliers
# output: a data.frame with columns y, x.1, ..., x.<n_coef>, inlier
make_ransac_data <- function(n_obs, n_coef, inlier_fraction = 0.7) {
  coef <- rep(1, n_coef)
  design <- matrix(runif(n_obs * n_coef), n_obs, n_coef)
  inlier <- sample(
    c(TRUE, FALSE)[c(
      rep(1, n_obs * inlier_fraction),
      rep(2, n_obs - n_obs * inlier_fraction)
    )]
  )
  # E(y) = design %*% coef if inlier, else random draw from
  # uniform dist. on [-10 * n_coef, -2 * n_coef] and [2 * n_coef, 10 * n_coef].
  # error variance = n_coef * Var(U[0,1])
  inlier_expected_y <- design %*% coef
  outlier_expected_y <- sample(c(-1, 1), n_obs, replace = TRUE) *
    runif(n_obs, min = 2 * n_coef, max = 10 * n_coef)
  y <- ifelse(inlier, inlier_expected_y, outlier_expected_y) +
    rnorm(n_obs, sd = sqrt(1 / 12 * n_coef))
  data.frame(y, x = design, inlier)
}

#-------------------------------------------------------------------------------

# summarize & visualize ransaclm() results on data from "make_ransac_data". Only
# works if ransaclm's output list has a "data" entry with columns "inlier" as
# generated by make_ransac_data() as well as a column ".consensus_set" flagging
# the observations in the consensus set found by RANSAC.
validate_ransac <- function(ransacmodel, plot = TRUE) {
  checkmate::assert_class(ransacmodel[["model"]], "lm")
  checkmate::assert_data_frame(ransacmodel[["data"]])
  data <- ransacmodel[["data"]]
  checkmate::assert_logical(data[, "inlier"], any.missing = FALSE)
  checkmate::assert_logical(data[, ".consensus_set"], any.missing = FALSE)

  consensus_set <- data[, ".consensus_set"]
  true_inliers <- data[, "inlier"]
  cat("Inlier:\n")
  print(table(true = true_inliers, estimated = consensus_set))
  cat("\nCoefficients: (should be intercept ~0, effects ~1)\n")
  print(summary(ransacmodel[["model"]])$coefficients)
  if (plot &
      (!all(c("x", "y") %in% names(data)))) {
    warning("Can't plot this data, expecting columns 'x' and 'y'.")
    plot <- FALSE
  }
  if (plot) {
    plot_ransac(ransacmodel, data)
  }
  invisible(ransacmodel)
}

plot_ransac <- function(ransacmodel, data) {
  colors <- c(
    black = rgb(0, 0, 0, .5), red = rgb(1, 0, 0, .5),
    blue = rgb(0, 0, 1, .5)
  )
  # scatterplot with true inliers in red, estimated inliers in blue
  # true regression line in black, RANSAC model in blue
  with(data,
       plot(x, y, col = colors[inlier + 1], pch = 19, bty = "n"))
  abline(c(0, 1), lwd = 2, col = colors[1])
  abline(ransacmodel$model, col = colors[3], lty = 2, lwd = 2)
  with(subset(data, .consensus_set),
       points(x, y, pch = 4, cex = 1.5, col = colors[3]))
  legend("top",
         lty = c(NA, NA, 1, 2, NA), lwd = c(NA, NA, 2, 2, NA),
         pch = c(19, 19, NA, NA, 4), col = colors[c(1, 2, 1, 3, 3)],
         legend = c(
           "'true' outliers", "'true' inliers", "true regression line",
           "RANSAC estimate", "RANSAC consensus set"
         ),
         cex = .7, bg = NA, inset = c(0, -.15), ncol = 3, xpd = NA
  )
}
