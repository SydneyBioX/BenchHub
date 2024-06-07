mpop3 <- function(x1, x2, y1, y2, mpop2_result, family, intercept, ...) {
  if (family == "cox") {
    intercept <- TRUE
  }

  if (length(mpop2_result) < 2) {
    x1_sub <- x1[, mpop2_result]
    x2_sub <- x2[, mpop2_result]

    if (family == "cox") {
      stop("Performing single-variable Cox regression is not currently supported by the MPOP package.")
    } else {
      mpop_mode <- "glm"
      model1 <- stats::glm(y1 ~ x1_sub, family = family)
      model2 <- stats::glm(y1 ~ x2_sub, family = family)
    }
  } else {
    mpop_mode <- "glmnet"
    model1 <- glmnet::cv.glmnet(
      x = x1[, mpop2_result],
      y = y1,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...
    )

    model2 <- glmnet::cv.glmnet(
      x = x2[, mpop2_result],
      y = y2,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...
    )
  }

  result <- list(
    mpop_mode = mpop_mode,
    model1 = model1,
    model2 = model2,
    feature = mpop2_result
  )
  return(result)
}
