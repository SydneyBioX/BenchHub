test_that("Trio can extract evidenceColumns directly from a source-backed dataset", {
  testCache <- tempdir()

  trio <- Trio$new(
    "figshare:26142922/47361079",
    evidenceColumns = c("time", "status"),
    task = "Risk Estimation",
    metrics = list(
      "Harrell C-index" = harrelCIndexMetric,
      "Begg C-index" = beggCIndexMetric
    ),
    cachePath = testCache
  )

  expect_true("Risk Estimation" %in% names(trio$evidence))
  expect_equal(
    colnames(trio$evidence[["Risk Estimation"]][["evidence"]]),
    c("time", "status")
  )
  expect_false(any(c("time", "status") %in% colnames(trio$data)))
  expect_equal(
    names(trio$metrics),
    c("Harrell C-index", "Begg C-index")
  )
})
