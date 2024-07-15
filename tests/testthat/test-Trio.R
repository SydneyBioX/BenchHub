fraction_zeros <- function(sce) {
  data <- data.frame(t(SummarizedExperiment::assay(sce, "counts")))
  sapply(data, function(col) sum(col == 0) / length(col))
}

test_that("Evaluation works.", {
  testCache <- system.file("extdata", "testdata", package = "TrioR")

  trio <- Trio$new("figshare:26054188/47112109", cachePath = testCache)

  sce <- trio |> purrr::pluck("data", 1)

  expected <- fraction_zeros(sce)

  trio$addGS("fracZero", fraction_zeros, c("KDE Score", "KDE Score 2"))
  trio$addGS("fracZero2", fraction_zeros, c("KDE Score", "KDE Score 2"))

  actual <- trio$getGS("fracZero") |> purrr::pluck(1)

  expect_equal(actual, expected)

  trio$addMetric("KDE Score", kdeMetric)
  trio$addMetric("KDE Score 2", kdeMetric)

  evaluation <- trio$evaluate(list(fracZero = actual, fracZero2 = actual))

  actual_eval <- kdeMetric(actual, actual)

  expect_equal(actual_eval, purrr::pluck(evaluation, "fracZero", "KDE Score"))
})


test_that("get data by name", {
  testCache <- system.file("extdata", "testdata", package = "TrioR")
  testthat::expect_no_error(Trio$new("MOBNEW", cachePath = testCache))

  testthat::expect_error(Trio$new("InvalidDatasetName"))
})
