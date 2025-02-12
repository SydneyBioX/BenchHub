test_that("getHeatmap handles multiple datasets with duplicate GS names by using metrics", {
  # Create evalResult structure with three datasets
  evalResult1 <- data.frame(
    datasetID = rep("dataset1", 15),
    method = c(rep("SRTsim", 5), rep("scDesign3", 5), rep("scDesign2", 5)),
    auxData = c("fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
                "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
                "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering"),
    metric = c("KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI"),
    result = c(0.5228492, 0.3692081, 0.3692081, 0.45, 0.65,
               0.1863305, 0.3556976, 1.333087, 0.35, 0.65,
               0.5456492, 0.4694581, 0.3688881, 0.48, 0.95)
  )
  
  benchmark <- benchmarkInsights$new(evalResult1)
  library(dplyr)
  evalResult2 <- data.frame(
    datasetID = c(rep("dataset2", 15)),
    method = c(rep("SRTsim", 5), rep("scDesign3", 5), rep("scDesign2", 5)),
    auxData = c("fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
           "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
           "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering"),
    metric = c("KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI"),
    result = c(0.7228492, 0.7892081, 0.1232081, 0.85, 0.78,
               0.1453305, 0.8956976, -1.9990870, 0.15, 0.28,
               0.3456620, 0.4634561, 0.3000010, 0.23, 0.12000008)
  )
  
  evalResult3 <- data.frame(
    datasetID = rep("dataset3", 15),
    method = c(rep("SRTsim", 5), rep("scDesign3", 5), rep("scDesign2", 5)),
    auxData = c("fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
           "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering",
           "fracZero_spot", "fracZero_gene", "libSize", "clustering", "clustering"),
    metric = c("KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI",
               "KS", "KS", "KS", "ARI", "NMI"),
    result = c(0.1238492, 0.5672081, 0.7892081, 0.78, 0.99,
               0.6666305, 0.2345976, 1.8888870, 0.89, 0.56,
               0.1236492, 0.4656780, 0.1234810, 0.23, 0.89)
  )
  
  # Add evaluation summaries
  benchmark$addevalSummary(evalResult2)
  benchmark$addevalSummary(evalResult3)
  
  # Generate heatmap and check it was created correctly
  heatmap <- benchmark$getHeatmap(benchmark$evalSummary)
  
  KDE_summary <- benchmark$evalSummary |>
    dplyr::filter(metric == "KS")
  
  grouped_boxplot <- benchmark$getBoxplot(KDE_summary)
  
  GS_corplot <- benchmark$getCorplot(benchmark$evalSummary, "auxData")
  metric_corplot <- benchmark$getCorplot(benchmark$evalSummary, "metric")
  Compare_corplot <- benchmark$getCorplot(benchmark$evalSummary, "method")
  
  forest_plot <- benchmark$getForestplot(benchmark$evalSummary, "metric", "method")

  
  # Ensure the heatmap object is not NULL
  expect_true(!is.null(heatmap))
  expect_true(!is.null(grouped_boxplot))
  expect_true(!is.null(forest_plot))
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
  evalResult1 <- data.frame(
    datasetID = rep("dataset1", 6),
    method = c(rep("SRTsim", 3), rep("scDesign3", 3)),
    auxData = c("1000_200", "2000_200", "3000_200", "1000_200", "2000_200", "3000_200"),
    metric = rep("time", 6),
    result = c(233, 543, 666, 567, 777, 890)
  )
  benchmark <- benchmarkInsights$new(evalResult1)

  evalResult2 <- data.frame(
    datasetID = rep("dataset2", 6),
    method = c(rep("SRTsim", 3), rep("scDesign3", 3)),
    auxData = c("1000_200", "2000_200", "3000_200", "1000_200", "2000_200", "3000_200"),
    metric = rep("time", 6),
    result = c(123, 678, 888, 456, 445, 789)
  )

  benchmark$addevalSummary(evalResult2)

  line_plot <- benchmark$getLineplot(benchmark$evalSummary, c('3000_200', '2000_200', '1000_200'))
  line_plot <- benchmark$getLineplot(benchmark$evalSummary)
  expect_true(inherits(line_plot, "ggplot"))
})

test_that("getScatterplot handles multiple datasets with duplicate GS names by using two metrics", {
  

  evalResult1 <- data.frame(
    datasetID = rep("dataset1", 12),
    method = c("SRTsim", "SRTsim", "scDesign3", "scDesign3", 
               "SPARsim", "SPARsim", "Spider", "Spider", 
               "powersimR", "powersimR", "SPsimSeq", "SPsimSeq"),
    auxData = rep("SVG", 12),
    metric = rep(c("sensitivity", "specificity"), 6),
    result = c(0.70, 0.80, 0.56, 0.70, 0.76, 0.88, 0.36, 0.45, 0.76, 0.88, 0.12, 0.23)
  )
  
  benchmark <- benchmarkInsights$new(evalResult1)
  
  evalResult2 <- data.frame(
    datasetID = rep("dataset2", 12),
    method = c("SRTsim", "SRTsim", "scDesign3", "scDesign3", 
               "SPARsim", "SPARsim", "Spider", "Spider", 
               "powersimR", "powersimR", "SPsimSeq", "SPsimSeq"),
    auxData = rep("SVG", 12),
    metric = rep(c("sensitivity", "specificity"), 6),
    result = c(0.67, 0.80, 0.45, 0.78, 0.56, 0.56, 0.89, 0.79, 0.45, 0.56, 0.32, 0.12)
  )
  
  # Add evaluation summaries
  benchmark$addevalSummary(evalResult2)
  
  # Generate scatterplot and check it was created correctly
  scatterplot <- benchmark$getScatterplot(benchmark$evalSummary)
  
  # Ensure the scatterplot object is not NULL
  expect_true(!is.null(scatterplot))
})









