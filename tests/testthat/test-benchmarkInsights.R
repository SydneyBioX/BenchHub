test_that("addevalSummary handles complex Compare structure correctly and is read-only", {
  benchmark <- benchmarkInsights$new()
  
  evalResult <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.5228492)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.3692081)),
      libSize = list(libSize = list("Kernel density score" = -0.3692081))
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.1863305)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.3556976)),
      libSize = list(libSize = list("Kernel density score" = -1.333087))
    )
  )
  
  dataID <- "dataset1"
  benchmark$addevalSummary(evalResult, dataID)
  
  # Check that the evalSummary has the correct number of rows (6 rows expected)
  expect_equal(nrow(benchmark$evalSummary), 6)
  
  # Check the structure and content of evalSummary
  expect_equal(benchmark$evalSummary$datasetID, rep("dataset1", 6))
  expect_equal(benchmark$evalSummary$GS, c("fracZero_spot", "fracZero_gene", "libSize", 
                                           "fracZero_spot", "fracZero_gene", "libSize"))
  expect_equal(benchmark$evalSummary$Compare, c("SRTsim", "SRTsim", "SRTsim", 
                                                "scDesign3", "scDesign3", "scDesign3"))
  expect_equal(benchmark$evalSummary$metric, rep("Kernel density score", 6))
  expect_equal(benchmark$evalSummary$result, c(-0.5228492, -0.3692081, -0.3692081, 
                                               -0.1863305, -0.3556976, -1.333087))
  
  # Check that attempting to modify addevalSummary throws an error with the correct message
  expect_error({
    benchmark$addevalSummary <- "attempt to modify"
  }, "cannot change value of locked binding for 'addevalSummary'")
})


