mpop2_sign <- function(x1, x2, y1, y2, family, mpop1_features, s = "lambda.min", nIter = 20,
                       mpop2_break = TRUE, intercept, ...) {
  p <- length(mpop1_features)

  if (p < 2) {
    warning("Only one feature (", mpop1_features, ") can be selected by the MPOP. Classical GLM results weill be given for this single feature model.")
    return(mpop1_features)
  }

  mpop2_features <- mpop1_features

  for (j in 1:nIter) {
    x1_reduced <- x1[, mpop2_features, drop = FALSE]
    x2_reduced <- x2[, mpop2_features, drop = FALSE]

    ridge1 <- glmnet::cv.glmnet(
      x = x1_reduced,
      y = y1,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...
    )

    coef1 <- glmnet::coef.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 <- sign(coef1)

    ridge2 <- glmnet::cv.glmnet(
      x = x2_reduced,
      y = y2,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...
    )

    coef2 <- glmnet::coef.glmnet(ridge2, s = s)[-1, , drop = FALSE]
    signCoef2 <- sign(coef2)

    mpop2_features <- mpop2_features[as.matrix(signCoef1 == signCoef2)]

    confTable <- table(
      factor(as.matrix(signCoef1), levels = c(-1, 0, 1)),
      factor(as.matrix(signCoef2), levels = c(-1, 0, 1))
    )

    confTable_diag0 <- confTable
    diag(confTable_diag0) <- 0
    message("MPOP2 - Sign: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(mpop2_features), " out of ", p)
    message("The sign matrix between the two data:")
    print(confTable_diag0)

    if (mpop2_break & sum(confTable_diag0) == 0) {
      break # if all features with discorded signs are removed then break
    }

    if (length(mpop2_features) == 0) {
      warning("MPOP step 2 removed all features, MPOP1 features will be used without any filtering")
      return(mpop1_features)
    }
  }
  return(mpop2_features)
}



mpop2_mag <- function(x1, x2, y1, y2, family, mpop1_features, s = "lambda.min", nIter = 20,
                      mpop2_break = FALSE, mag = 1, intercept, ...) {
  p <- length(mpop1_features)
  mpop2_features <- mpop1_features

  for (j in 1:nIter) {
    if (length(mpop2_features) <= 10) {
      break
    }

    x1_reduced <- x1[, mpop2_features, drop = FALSE]
    x2_reduced <- x2[, mpop2_features, drop = FALSE]

    ridge1 <- glmnet::cv.glmnet(
      x = x1_reduced,
      y = y1,
      family = family,
      alpha = 0, intercept = intercept, ...
    )

    coef1 <- glmnet::coef.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 <- sign(coef1)

    ridge2 <- glmnet::cv.glmnet(
      x = x2_reduced,
      y = y2,
      family = family,
      alpha = 0, intercept = intercept, ...
    )

    coef2 <- glmnet::coef.glmnet(ridge2, s = s)[-1, , drop = FALSE]
    signCoef2 <- sign(coef2)

    criterion <- (1 / sqrt(2)) * abs(coef1 - coef2)
    mpop2_features <- mpop2_features[as.vector(criterion <= mag)]
    message("MPOP2 - Mag: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(mpop2_features), " out of ", p)

    if (mpop2_break) {
      break
    }
  } ## End j-loop
  return(mpop2_features)
}
