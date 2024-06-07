predict_mpop = function(mpop_result, newx, s = "lambda.min") {
  # if any discovered feature set is not in newx, stop
  assertthat::assert_that(all(mpop_result$feature %in% colnames(newx)),
                          msg = "mpop_result feature must be a strict subset of colnames(newx)")
  
  assertthat::assert_that(sum(is.na(newx)) == 0,
  msg = "All entries of the prediction data must be non-missing.
  You should try running an imputation method on the input matrix.
  One option is to use `impute_mpop`")
  
  if (is.null(mpop_result)) {
    return(NULL)
  }
  
  if (mpop_result$mpop_mode == "glmnet") {
    result1_link = predict(object = mpop_result$model1, newx = newx[, mpop_result$feature, drop = FALSE], s = s, type = "link")
    result2_link = predict(object = mpop_result$model2, newx = newx[, mpop_result$feature, drop = FALSE], s = s, type = "link")
  } else {
    result1_link = stats::predict(object = mpop_result$model1, newx = newx[, mpop_result$feature, drop = FALSE], type = "link")
    result2_link = stats::predict(object = mpop_result$model2, newx = newx[, mpop_result$feature, drop = FALSE], type = "link")
  }
  
  result_mat = cbind(result1_link, result2_link, (result1_link + result2_link)/2)
  colnames(result_mat) = c("mpop_model1", "mpop_model2", "mpop_model_avg")
  
  if (is.null(rownames(result_mat))) {
    rownames(result_mat) = 1:nrow(result_mat)
  }
  
  tib_result = tibble::as_tibble(data.frame(result_mat))
  tib_result = dplyr::mutate(tib_result, samples = rownames(result_mat))
  
  if (mpop_result$family_params$family == "binomial") {
    if (mpop_result$mpop_mode == "glmnet") {
      result1_prob = predict(object = mpop_result$model1, newx = newx[, mpop_result$feature, drop = FALSE], s = s, type = "response")
      result2_prob = predict(object = mpop_result$model2, newx = newx[, mpop_result$feature, drop = FALSE], s = s, type = "response")
    } else {
      result1_prob = predict(object = mpop_result$model1, newx = newx[, mpop_result$feature, drop = FALSE], type = "response")
      result2_prob = predict(object = mpop_result$model2, newx = newx[, mpop_result$feature, drop = FALSE], type = "response")
    }
    
    mpop_model_avg_prob = (as.vector(result1_prob) + as.vector(result2_prob))/2
    mpop_model_avg_class = dplyr::case_when(mpop_model_avg_prob <= 0.5 ~ mpop_result$family_params$factor_levels[1],
                                            mpop_model_avg_prob > 0.5 ~ mpop_result$family_params$factor_levels[2],
                                            TRUE ~ NA_character_)
    
    tib_result = tib_result %>% 
      dplyr::mutate(mpop_model_avg_prob = mpop_model_avg_prob,
                    mpop_model_avg_class = mpop_model_avg_class)
  }
  tib_result = dplyr::select(tib_result, .data$samples, dplyr::everything())
  return(tib_result)
}



impute_mpop = function(mpop_result, x1, x2, newx) {
  mpop_taxa = sort(unique(mpop_result$feature))
  
  assertthat::assert_that(identical(colnames(x1), colnames(x2)),
                          msg = "The columns of x1 must be the same as x2")
  cols_intersect_x1x2_newx = intersect(colnames(x1), colnames(newx))
  
  x1_intersect_newx = x1[, cols_intersect_x1x2_newx, drop = FALSE]
  x2_intersect_newx = x2[, cols_intersect_x1x2_newx, drop = FALSE]
  
  miss_data = newx
  miss_data_intersect_x1x2 = miss_data[, cols_intersect_x1x2_newx, drop = FALSE]
  which_col_missing = colSums(is.na(miss_data_intersect_x1x2)) > 0 ## Which column has any missing values
  miss_names = colnames(miss_data_intersect_x1x2)[which_col_missing, drop = FALSE]
  
  for (this_miss_col in miss_names) {
    # complete data
    x1_without_miss = x1_intersect_newx[, !which_col_missing, drop = FALSE]
    x2_without_miss = x2_intersect_newx[, !which_col_missing, drop = FALSE]
    
    g_x1 = glmnet::cv.glmnet(x = x1_without_miss,
                             y = x1[, this_miss_col, drop = FALSE],
                             family = "gaussian", nfolds = 5)
    
    g_x2 = glmnet::cv.glmnet(x = x2_without_miss,
                             y = x2[, this_miss_col, drop = FALSE],
                             family = "gaussian", nfolds = 5)
    x1_pred = stats::predict(g_x1, newx = cbind(miss_data_intersect_x1x2[, !which_col_missing, drop = FALSE]))
    x2_pred = stats::predict(g_x2, newx = cbind(miss_data_intersect_x1x2[, !which_col_missing, drop = FALSE]))
    miss_data_intersect_x1x2[, this_miss_col] = as.numeric(x1_pred + x2_pred)/2
  }
  return(miss_data_intersect_x1x2)
}