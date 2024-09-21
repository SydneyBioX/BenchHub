test_that("getHeatmap handles multiple datasets with duplicate GS names by using metrics", {
  benchmark <- benchmarkInsights$new()
  
  # Create evalResult structure with three datasets
  evalResult1 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.5228492)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.3692081)),
      libSize = list(libSize = list("KS" = -0.3692081)),
      clustering = list(
        ARI = list("ARI" = 0.45),
        NMI = list("NMI" = 0.65)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.1863305)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.3556976)),
      libSize = list(libSize = list("KS" = -1.333087)),
      clustering = list(
        ARI = list("ARI" = 0.35),
        NMI = list("NMI" = 0.65)
      )
    ),
    scDesign2 = list(
      fracZero_spot = list(fracZero_spot = list("KS" = 0.5456492)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.4694581)),
      libSize = list(libSize = list("KS" = 0.3688881)),
      clustering = list(
        ARI = list("ARI" = 0.48),
        NMI = list("NMI" = 0.95)
      )
    )
  )
  
  evalResult2 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.7228492)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.7892081)),
      libSize = list(libSize = list("KS" = -0.1232081)),
      clustering = list(
        ARI = list("ARI" = 0.85),
        NMI = list("NMI" = 0.78)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.1453305)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.8956976)),
      libSize = list(libSize = list("KS" = -1.999087)),
      clustering = list(
        ARI = list("ARI" = 0.15),
        NMI = list("NMI" = 0.28)
      )
    ),
      scDesign2 = list(
        fracZero_spot = list(fracZero_spot = list("KS" = 0.345662)),
        fracZero_gene = list(fracZero_gene = list("KS" = 0.4634561)),
        libSize = list(libSize = list("KS" = 0.300001)),
        clustering = list(
          ARI = list("ARI" = 0.23),
          NMI = list("NMI" = 0.12)
        )
      )
    
  )
  
  evalResult3 <- list(
    SRTsim = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.1238492)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.5672081)),
      libSize = list(libSize = list("KS" = -0.7892081)),
      clustering = list(
        ARI = list("ARI" = 0.78),
        NMI = list("NMI" = 0.99)
      )
    ),
    scDesign3 = list(
      fracZero_spot = list(fracZero_spot = list("KS" = -0.6666305)),
      fracZero_gene = list(fracZero_gene = list("KS" = -0.2345976)),
      libSize = list(libSize = list("KS" = -1.888887)),
      clustering = list(
        ARI = list("ARI" = 0.89),
        NMI = list("NMI" = 0.56)
      )
    ),
    scDesign2 = list(
      fracZero_spot = list(fracZero_spot = list("KS" = 0.1236492)),
      fracZero_gene = list(fracZero_gene = list("KS" = 0.465678)),
      libSize = list(libSize = list("KS" = 0.123481)),
      clustering = list(
        ARI = list("ARI" = 0.23),
        NMI = list("NMI" = 0.89)
      )
    )
  )
  
  # Add evaluation summaries
  benchmark$addevalSummary(evalResult1, "dataset1")
  benchmark$addevalSummary(evalResult2, "dataset2")
  benchmark$addevalSummary(evalResult3, "dataset3")
  
  # Generate heatmap and check it was created correctly
  heatmap <- benchmark$getHeatmap(benchmark$evalSummary)
  
  KDE_summary <- benchmark$evalSummary |>
    dplyr::filter(metric == "KS")
  
  grouped_boxplot <- benchmark$getBoxplot(KDE_summary)
  
  GS_corplot <- benchmark$getCorplot(benchmark$evalSummary, "GS")
  metric_corplot <- benchmark$getCorplot(benchmark$evalSummary, "metric")
  Compare_corplot <- benchmark$getCorplot(benchmark$evalSummary, "Compare")
  
  forest_plot <- benchmark$getForestplot(benchmark$evalSummary, "metric", "Compare")

  
  # Ensure the heatmap object is not NULL
  expect_true(!is.null(heatmap))
  expect_true(!is.null(grouped_boxplot))
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

  line_plot <- benchmark$getLineplot(benchmark$evalSummary, c('3000_200', '2000_200', '1000_200'))
  line_plot <- benchmark$getLineplot(benchmark$evalSummary)
  expect_true(inherits(line_plot, "ggplot"))
})

test_that("getScatterplot handles multiple datasets with duplicate GS names by using two metrics", {
  benchmark <- benchmarkInsights$new()

  evalResult1 <- list(
    SRTsim = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.7),
        specificity = list("specificity" = 0.8)
      )
    ),
    scDesign3 = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.56),
        specificity = list("specificity" = 0.7)
      )
    ),
    SPARsim = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.76),
        specificity = list("specificity" = 0.88)
      )
    ),
    Spider = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.36),
        specificity = list("specificity" = 0.45)
      )
    ),
    powersimR = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.76),
        specificity = list("specificity" = 0.88)
      )
    ),
    SPsimSeq = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.12),
        specificity = list("specificity" = 0.23)
      )
    )
  )
  
  evalResult2 <- list(
    SRTsim = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.67),
        specificity = list("specificity" = 0.80)
      )
    ),
    scDesign3 = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.45),
        specificity = list("specificity" = 0.78)
      )
    ),
    SPARsim = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.56),
        specificity = list("specificity" = 0.56)
      )
    ),
    Spider = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.89),
        specificity = list("specificity" = 0.79)
      )
    ),
    powersimR = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.45),
        specificity = list("specificity" = 0.56)
      )
    ),
    SPsimSeq = list(
      SVG = list(
        sensitivity = list("sensitivity" = 0.32),
        specificity = list("specificity" = 0.12)
      )
    )
  )
  
  # Add evaluation summaries
  benchmark$addevalSummary(evalResult1, "dataset1")
  benchmark$addevalSummary(evalResult2, "dataset2")
  
  # Generate scatterplot and check it was created correctly
  scatterplot <- benchmark$getScatterplot(benchmark$evalSummary)
  
  # Ensure the scatterplot object is not NULL
  expect_true(!is.null(scatterplot))
})



library(dotwhisker)
library(tidyverse)
library(broom)



to_plot <-  benchmark$evalSummary  %>%
  group_by(metric) %>%
  do(broom::tidy(lm( result ~  Compare, data = .))) %>%
  rename(model = metric) %>%  
  relabel_predictors(c(
    '(Intercept)' = "ComparescDesign2"))

g <- dotwhisker::dwplot(to_plot,
            vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + theme_minimal()  + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill=NA)) 


to_plot <-  benchmark$evalSummary  %>%
  group_by(datasetID) %>%
  do(broom::tidy(lm( result ~ Compare, data = .))) %>%
  rename(model = datasetID) %>%  
  relabel_predictors(c(
    '(Intercept)' = "metric"))

g <- dotwhisker::dwplot(to_plot,
                        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + theme_minimal()  + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill=NA)) 








