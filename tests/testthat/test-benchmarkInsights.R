test_that("addevalSummary handles complex Compare structure correctly and metadata works as expected", {
  benchmark <- benchmarkInsights$new()
  
  # Create complex evalResult structure
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
  
  # Add evaluation summary
  benchmark$addevalSummary(evalResult, dataID)
  
  # Check that evalSummary has the correct number of rows (6 rows expected)
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
  
  # Create a metadata dataframe
  metadata_df <- data.frame(
    dataID = "dataset1",
    speciesTissue = "Human breast cancer",
    healthState = "Spatial",
    protocol = "Visium",
    Spot_cell_number = 4744,
    Gene_number = 28402,
    DOI = "10.1038/s41588-021-00911-1",
    stringsAsFactors = FALSE
  )
  
  # Add metadata
  benchmark$addMetadata(metadata_df)
  
  # Check that the metadata is added correctly
  expect_equal(nrow(benchmark$metadata), 1)
  expect_equal(benchmark$metadata$dataID, "dataset1")
  expect_equal(benchmark$metadata$speciesTissue, "Human breast cancer")
  expect_equal(benchmark$metadata$healthState, "Spatial")
  expect_equal(benchmark$metadata$protocol, "Visium")
  expect_equal(benchmark$metadata$Spot_cell_number, 4744)
  expect_equal(benchmark$metadata$Gene_number, 28402)
  expect_equal(benchmark$metadata$DOI, "10.1038/s41588-021-00911-1")
})


