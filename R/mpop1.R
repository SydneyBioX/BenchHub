mpop1 <- function(x1, x2, y1, y2, family, alpha = 1,
                  n_iter = 20, n_features = 50, s = "lambda.min",
                  mpop1_method = "normal", ...) {
  # weights for Zero-Sum regression
  w1 <- rep(0, length(y1))
  w1[y1 == 0] <- rep(1 / sum(y1 == 0), sum(y1 == 0))
  w1[y1 == 1] <- rep(1 / sum(y1 == 1), sum(y1 == 1))

  w2 <- rep(0, length(y2))
  w2[y2 == 0] <- rep(1 / sum(y2 == 0), sum(y2 == 0))
  w2[y2 == 1] <- rep(1 / sum(y2 == 1), sum(y2 == 1))

  # initialise the selected feature set
  p <- ncol(x1)
  remaining_features <- colnames(x1)
  step_features <- vector("list", length = n_iter)
  selected_features <- do.call(c, step_features)

  for (i in 1:n_iter) {
    # print out the number of selected features at each iteration
    message(
      "MPOP1 - Step ", sprintf("%02d", i),
      ": Number of selected features: ",
      length(selected_features),
      " out of ", p
    )

    # if we exceed the number of desired features, stop iterations
    if (length(selected_features) >= n_features) {
      message(n_features, " features have been reached. ")
      message("A total of ", length(selected_features), " features have been selected. \n")
      break
    }

    # if we exhaust all features, stop iterations
    if (length(selected_features) == p) {
      message("All features have been selected, break now.")
      break
    }

    # Zero-Sum model 1 for x1 and y1
    # fit the data with only the remaining features
    zs1 <- zeroSum::zeroSum(x1[, remaining_features, drop = FALSE],
      y1,
      weights = w1,
      alpha = alpha,
      family = family
    )

    # Zero-Sum model 2 for x2 and y2
    # fit the data with only the remaining features
    zs2 <- zeroSum::zeroSum(x2[, remaining_features, drop = FALSE],
      y2,
      weights = w2,
      alpha = alpha,
      family = family
    )

    # the selected feature set is a concatenation of
    # existing selected features with common features jointly selected by
    # the two Zero-Sum models
    zs1_coef <- stats::coef(zs1, s = s)[, 1][stats::coef(zs1, s = s)[, 1] != 0] %>% as.matrix()
    colnames(zs1_coef) <- s
    rownames(zs1_coef)[1] <- "(Intercept)"

    zs2_coef <- stats::coef(zs2, s = s)[, 1][stats::coef(zs2, s = s)[, 1] != 0] %>% as.matrix()
    colnames(zs2_coef) <- s
    rownames(zs2_coef)[1] <- "(Intercept)"

    zs1_coef_tbl <- feature_tibble(zs1_coef, coef_model = "1")
    zs2_coef_tbl <- feature_tibble(zs2_coef, coef_model = "2")

    # collect all the selected features by the two Zero-Sum regression models
    step_features[[i]] <- dplyr::bind_rows(zs1_coef_tbl, zs2_coef_tbl)

    # features selected in this step of MPOP
    selected_features <- c(
      selected_features,
      base::intersect(rownames(zs1_coef), rownames(zs2_coef))
    ) %>% unique()
    selected_features <- selected_features[selected_features != "(Intercept)"]

    # remaining features are the features that are not in the selected feature set
    remaining_features <- setdiff(colnames(x1), selected_features)
  }

  # finalise the feature set
  step_features_tbl <- dplyr::bind_rows(step_features, .id = "step")

  # if there are no features selected
  if (length(selected_features) == 0 & mpop1_method == "after") {
    warning("No predictive features commonly predictive in both data (at eacrmh iteration) were found \n alternative feature set was be used")
    message("Features ever selected by both data (after all iterations) will now be pooled")

    selected_features <- step_features_tbl %>%
      dplyr::filter(.data$feature_name != "(Intercept)") %>%
      dplyr::select(.data$coef_model, .data$feature_name) %>%
      dplyr::distinct(.data$coef_model, .data$feature_name) %>%
      dplyr::group_by(.data$feature_name) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 2) %>%
      dplyr::pull(.data$feature_name)
  } else if (length(selected_features) == 0 & mpop1_method == "either") {
    warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
    message("Features ever selected by either data will now be pooled")

    selected_features <- step_features_tbl %>%
      dplyr::filter(.data$feature_name != "(Intercept)") %>%
      dplyr::pull(.data$feature_name) %>%
      unique()
  }

  if (length(selected_features) == 0) {
    return(NULL)
  } else {
    final_features <- selected_features
    message("Removing sources of collinearity gives ", length(final_features), " features. \n")

    step_features_tbl <- step_features_tbl %>%
      dplyr::mutate(
        ever_selected_features = .data$feature_name %in% selected_features,
        in_final_features = .data$feature_name %in% final_features
      )

    return(tibble::lst(final_features, step_features))
  }
}



feature_tibble <- function(zs_coef, coef_model = NA) {
  if (nrow(zs_coef) == 0) {
    zs_coef_tbl <- tibble::tibble(
      coef_model = coef_model,
      feature_name = NA,
      feature_value = NA
    )
  } else {
    zs_coef_tbl <- tibble::tibble(
      coef_model = coef_model,
      feature_name = rownames(zs_coef),
      feature_value = as.vector(as.matrix(zs_coef))
    )
  }
  return(zs_coef_tbl)
}



mpop1_iterate <- function(x1, x2, y1, y2, family, alpha = 1,
                          n_iter = 20, n_features = 50,
                          s = "lambda.min", ...) {
  alpha <- sort(alpha, decreasing = TRUE)
  all_selected_features <- c()

  for (i in alpha) {
    message("Fitting an MPOP model using alpha = ", i, "\n")

    mpop1_result <- mpop1(
      x1 = x1, x2 = x2, y1 = y1, y2 = y2,
      n_iter = n_iter, n_features = n_features, alpha = i,
      family = family, s = s, ...
    )

    all_selected_features <- unique(c(all_selected_features, mpop1_result$final_features))

    if (length(all_selected_features) >= n_features) {
      message(n_features, " features have been reached. ")
      message("A total of ", length(all_selected_features), " features were selected. \n")
      break
    } else {
      message(n_features, " features have not been reached. \n")
    }

    if (length(all_selected_features) == 0) {
      return(NULL)
    } else {
      return(tibble::lst(
        mpop1_features = all_selected_features,
        step_features = mpop1_result$step_features
      ))
    }
  }
}
