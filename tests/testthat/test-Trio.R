fraction_zeros <- function(sce) {
  rowMeans(SummarizedExperiment::assay(sce, "counts") > 0)
}

test_that("Evaluation works.", {
  testCache <- tempdir()

  trio <- Trio$new("figshare:26054188/47112109", cachePath = testCache)

  sce <- trio |> purrr::pluck("data")

  expected <- fraction_zeros(sce)

  trio$addAuxData("fracZero", fraction_zeros, c("KDE Score", "KDE Score 2"))
  trio$addAuxData("fracZero2", fraction_zeros, c("KDE Score", "KDE Score 2"))

  actual <- trio$getAuxData("fracZero")

  expect_equal(actual, expected)

  trio$addMetric("KDE Score", kdeMetric)
  trio$addMetric("KDE Score 2", kdeMetric)

  evaluation <- trio$evaluate(list(fracZero = actual, fracZero2 = actual))

  actual_eval <- kdeMetric(actual, actual)

  testthat::expect_equal(
    evaluation[1, "result"][[1]][[1]], actual_eval
  )

  # Separate methods evaluation
  testthat::expect_no_error(
    evaluation <- trio$evaluate(
      list(
        method1 = list(
          fracZero = actual,
          fracZero2 = actual
        ),
        method2 = list(
          fracZero = actual,
          fracZero2 = actual
        )
      )
    )
  )
})


test_that("get data by name", {
  testCache <- tempdir()
  testthat::expect_warning(Trio$new("MOBNEW", cachePath = testCache))

  testthat::expect_error(Trio$new("InvalidDatasetName"))
})
