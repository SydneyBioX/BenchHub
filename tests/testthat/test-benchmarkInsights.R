test_that("getHeatmap handles multiple datasets with duplicate GS names by using metrics", {
  benchmark <- benchmarkInsights$new()
  
  # Create evalResult structure with three datasets
  evalResult1 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.5228492)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.3692081)),
      libSize = list(libSize = list("Kernel density score" = -0.3692081)),
      clustering = list(
        ARI = list("ARI" = 0.45),
        NMI = list("NMI" = 0.65)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.1863305)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.3556976)),
      libSize = list(libSize = list("Kernel density score" = -1.333087)),
      clustering = list(
        ARI = list("ARI" = 0.35),
        NMI = list("NMI" = 0.65)
      )
    )
  )
  
  evalResult2 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.7228492)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.7892081)),
      libSize = list(libSize = list("Kernel density score" = -0.1232081)),
      clustering = list(
        ARI = list("ARI" = 0.85),
        NMI = list("NMI" = 0.78)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.1453305)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.8956976)),
      libSize = list(libSize = list("Kernel density score" = -1.999087)),
      clustering = list(
        ARI = list("ARI" = 0.15),
        NMI = list("NMI" = 0.28)
      )
    )
  )
  
  evalResult3 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.1238492)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.5672081)),
      libSize = list(libSize = list("Kernel density score" = -0.7892081)),
      clustering = list(
        ARI = list("ARI" = 0.78),
        NMI = list("NMI" = 0.99)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("Kernel density score" = -0.6666305)),
      fracZero_gene = list(fracZero_gene = list("Kernel density score" = -0.2345976)),
      libSize = list(libSize = list("Kernel density score" = -1.888887)),
      clustering = list(
        ARI = list("ARI" = 0.89),
        NMI = list("NMI" = 0.56)
      )
    )
  )
  
  # Add evaluation summaries
  benchmark$addevalSummary(evalResult1, "dataset1")
  benchmark$addevalSummary(evalResult2, "dataset2")
  benchmark$addevalSummary(evalResult3, "dataset3")
  
  # Generate heatmap and check it was created correctly
  heatmap <- benchmark$getHeatmap(benchmark$evalSummary)
  
  # Ensure the heatmap object is not NULL
  expect_true(!is.null(heatmap))
})


test_that("addMetadata handles multiple entries correctly", {
  benchmark <- benchmarkInsights$new()
  
  # Create metadata dataframe
  metadata_df1 <- data.frame(
    dataID = "dataset1",
    speciesTissue = "Human breast cancer",
    healthState = "Spatial",
    protocol = "Visium",
    Spot_cell_number = 4744,
    Gene_number = 28402,
    DOI = "10.1038/s41588-021-00911-1",
    stringsAsFactors = FALSE
  )
  
  metadata_df2 <- data.frame(
    dataID = "dataset2",
    speciesTissue = "Mouse brain",
    healthState = "Healthy",
    protocol = "ST",
    Spot_cell_number = 3750,
    Gene_number = 25000,
    DOI = "10.1016/j.cell.2021.05.015",
    stringsAsFactors = FALSE
  )
  
  # Add both metadata dataframes
  benchmark$addMetadata(metadata_df1)
  benchmark$addMetadata(metadata_df2)
  
  # Check that metadata has two rows
  expect_equal(nrow(benchmark$metadata), 2)
  
  # Verify content of the first and second rows
  expect_equal(benchmark$metadata$dataID, c("dataset1", "dataset2"))
  expect_equal(benchmark$metadata$speciesTissue, c("Human breast cancer", "Mouse brain"))
  expect_equal(benchmark$metadata$protocol, c("Visium", "ST"))
  expect_equal(benchmark$metadata$DOI, c("10.1038/s41588-021-00911-1", "10.1016/j.cell.2021.05.015"))
})

test_that("getScatterplot creates a scatterplot with optional grouping", {
  benchmark <- benchmarkInsights$new()
  evalResult1 <- list(
    SRTsim = list(
      "1000_200" = list(time = list("time" = 233)),
      "2000_200" = list(time = list("time" = 543)),
      "3000_200" = list(time = list("time" = 666))
    ),
    scDesign3 = list(
      "1000_200" = list(time = list("time" = 567)),
      "2000_200" = list(time = list("time" = 777)),
      "3000_200" = list(time = list("time" = 890))
    )
  )
  
  evalResult2 <- list(
    SRTsim = list(
      "1000_200" = list(time = list("time" = 123)),
      "2000_200" = list(time = list("time" = 678)),
      "3000_200" = list(time = list("time" = 888))
    ),
    scDesign3 = list(
      "1000_200" = list(time = list("time" = 456)),
      "2000_200" = list(time = list("time" = 445)),
      "3000_200" = list(time = list("time" = 789))
    )
  )

  benchmark$addevalSummary(evalResult1, "dataset1")
  benchmark$addevalSummary(evalResult2, "dataset2")

  scatter_plot <- benchmark$getScatterplot(benchmark$evalSummary)
  expect_true(inherits(scatter_plot, "ggplot"))

})


