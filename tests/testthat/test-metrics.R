test_that("RMSEmetric is the square root of MSEmetric", {
  evidence <- c(1, 2, 3, 4)
  predicted <- c(1.1, 2.1, 2.9, 4.2)

  expect_equal(RMSEmetric(evidence, predicted), sqrt(MSEmetric(evidence, predicted)))
})

test_that("ARImetric and NMImetric return 1 for identical labelings", {
  evidence <- factor(c("A", "A", "B", "B"))
  predicted <- factor(c("A", "A", "B", "B"))

  expect_equal(ARImetric(evidence, predicted), 1)
  expect_equal(NMImetric(evidence, predicted), 1)
})

test_that("JSDmetric returns 0 for identical distributions", {
  evidence <- c(0.2, 0.3, 0.5)
  predicted <- c(0.2, 0.3, 0.5)

  expect_equal(JSDmetric(evidence, predicted), 0)
})
