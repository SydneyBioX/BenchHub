mpop_model = function(x1, x2, y1, y2, n_features = 50, n_iter = 20,
                      alpha = 1, family = "binomial", s = "lambda.min",
                      mpop2_break = TRUE, mpop2_type = "sign", mpop2_mag = 1,
                      mpop1_method = "normal", intercept = FALSE, ...) {
  # check inputs
  assertthat::assert_that(nrow(x1) == length(y1))
  assertthat::assert_that(nrow(x2) == length(y2))
  
  # for Zero-Sum regression y must be a numeric vector
  if (family == "binomial") {
    assertthat::assert_that(is.numeric(y1))
    assertthat::assert_that(is.numeric(y2))
  }
  
  mpop1_result = mpop1_iterate(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                               n_features = n_features, n_iter = n_iter,
                               alpha = alpha, s = s, family = family, 
                               mpop1_method = mpop1_method)
  mpop1_features = mpop1_result$mpop1_features
  
  if (length(mpop1_features) == 0) {
    warning("No predictive features were selected in Step 1. Return NULL.")
    return(NULL)
  }
  
  # after Zero-Sum regression,
  # convert the numeric vector y to a factor vector for CPOP
  y1 = as.factor(y1)
  y2 = as.factor(y2)
  factor_levels = levels(y1)
  
  if (mpop2_type == "sign") {
    mpop2_result = mpop2_sign(x1 = x1, x2 = x2, y1 = y1, y2 = y2, s = s,
                              mpop1_features = mpop1_features, n_iter = n_iter, 
                              family = family, mpop2_break = mpop2_break, intercept = intercept)
  }
  
  if (mpop2_type == "mag") {
    mpop2_result = mpop2_mag(x1 = x1, x2 = x2, y1 = y1, y2 = y2, s = s,
                             mpop1_features = mpop1_features, n_iter = n_iter,
                             family = family, mpop2_break = FALSE, mag = mpop2_mag, intercept = intercept)
  }
  
  if (length(mpop2_result) == 0) {
    warning("No predictive features were selected in Step 2. Return NULL.")
    return(NULL)
  }
  
  mpop3_result = mpop3(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                       mpop2_result = mpop2_result,
                       family = family, intercept = intercept)
  
  if (mpop3_result$mpop_mode == "glmnet") {
    coef1 = glmnet::coef.glmnet(mpop3_result$model1, s = s)
    coef2 = glmnet::coef.glmnet(mpop3_result$model2, s = s)
    coef_tbl = tibble::tibble(coef_name = rownames(coef1),
                              coef1 = as.vector(coef1),
                              coef2 = as.vector(coef2))
  } else {
    coef1 = stats::coefficients(mpop3_result$model1)
    coef2 = stats::coefficients(mpop3_result$model2)
    coef_tbl = tibble::tibble(coef_name = names(coef1),
                              coef1 = as.vector(coef1),
                              coef2 = as.vector(coef2))
  }
  
  result = c(mpop3_result,
             coef_tbl = list(coef_tbl),
             cpop1_features = list(mpop1_features),
             step_features = list(mpop1_result$step_features),
             family_params = list(list(family = family,
                                       factor_levels = factor_levels)),
             x1 = list(x1),
             x2 = list(x2))
  
  class(result) = c("mpop")
  return(result)
}

print.mpop = function(x, ...) {
  cat("MPOP model with ", length(x$feature), "features \n")
  print(x$coef_tbl)
}