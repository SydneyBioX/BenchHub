fraction_zeros <- function(sce) {
  data <- data.frame(t(SummarizedExperiment::assay(sce, "counts")))
  data.frame(sapply(data, function(col) sum(col == 0) / length(col)))
}


test_that("Gold standards work.", {
  trio <- Trio$new("figshare:26054188/47112109") |>
    suppressMessages()

  sce <- trio |>
    purrr::pluck("data", 1) |>
    suppressMessages()

  expected <- fraction_zeros(sce)

  trio$addGS("fracZero", fraction_zeros, "KDE Score")
  actual <- trio$getGS("fracZero")

  expect_equal(actual[[1]], expected)
})
