plot_mpop <- function(mpop_result, type = "point", s = "lambda.min") {
  assertthat::assert_that(type %in% c("point", "text", "bar"))
  coef1 <- glmnet::coef.glmnet(mpop_result$model1, s = s)
  coef2 <- glmnet::coef.glmnet(mpop_result$model2, s = s)

  stopifnot(identical(
    rownames(coef1),
    rownames(coef2)
  ))

  coef_plotdf <- tibble::tibble(
    coef_name = rownames(coef1),
    coef1 = as.vector(coef1),
    coef2 = as.vector(coef2)
  ) %>%
    dplyr::mutate(coef_name = forcats::fct_reorder(.data$coef_name, coef1)) %>%
    dplyr::filter(coef1 != 0, coef2 != 0)

  if (type == "point") {
    g1 <- coef_plotdf %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = .data$coef1, y = .data$coef2) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "red")

    return(lst(plot = g1, data = coef_plotdf))
  }

  if (type == "bar") {
    coef_plotdf_gather <- tidyr::pivot_longer(
      data = coef_plotdf,
      cols = c("coef1", "coef2"),
      names_to = "coef_key",
      values_to = "coef_value"
    )

    p <- coef_plotdf_gather %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = .data$coef_name, y = .data$coef_value, fill = .data$coef_key) +
      ggplot2::geom_col(position = "dodge")

    return(lst(plot = p, data = coef_plotdf_gather))
  }

  if (type == "text") {
    p <- coef_plotdf %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = .data$coef1, y = .data$coef2, label = .data$coef_name) +
      ggplot2::geom_text() +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "red")

    return(lst(plot = p, data = coef_plotdf))
  }
}
