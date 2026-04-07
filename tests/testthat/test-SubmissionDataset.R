submission_dataset_source <- local({
  candidates <- c(
    file.path("R", "SubmissionDataset.R"),
    file.path("..", "..", "R", "SubmissionDataset.R")
  )
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop("Could not locate R/SubmissionDataset.R for tests.")
  }

  normalizePath(existing[[1]], mustWork = TRUE)
})

testthat::test_that("buildTrioSubmission supports multiple evidence rows per task", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = c("RMSE", "Pearson")
    ),
    marker_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "Pearson"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      RMSE = MSEmetric,
      Pearson = function(evidence, predicted) stats::cor(evidence, predicted)
    )
  )

  trio$dataSource <- "GEO"
  trio$dataSourceID <- "GSE12345"
  trio$description <- "Breast cancer spatial transcriptomics dataset"
  trio$evidenceSourceID <- c(
    manual_annotation = "PMID123456",
    marker_annotation = "DOI:10.xxx"
  )

  submission <- buildTrioSubmission(
    trio = trio,
    dataset_args = list(
      name = "BREAST_ST",
      dataType = "omics",
      dataModality = "other",
      technology = "10x Visium",
      description = "Breast cancer spatial transcriptomics dataset",
      doi = "10.1000/test",
      tissue = "breast",
      status = "diseased"
    ),
    task_args = list(
      taskStage = "downstream",
      taskType = "deconvolution",
      taskName = "Cell type deconvolution"
    ),
    evidence_task_map = c(
      manual_annotation = "Cell type deconvolution",
      marker_annotation = "Cell type deconvolution"
    )
  )

  expect_true(is.na(submission$Dataset$datasetID))
  expect_equal(nrow(submission$DatasetTask), 1)
  expect_true(all(is.na(submission$DatasetTask$datasetTaskID)))
  expect_equal(nrow(submission$DatasetEvidence), 2)
  expect_true(all(is.na(submission$DatasetEvidence$datasetTaskID)))
  expect_setequal(
    submission$DatasetEvidence$supportingEvidence,
    c("manual_annotation", "marker_annotation")
  )
  expect_setequal(
    submission$DatasetEvidence$evidenceSourceID,
    c("PMID123456", "DOI:10.xxx")
  )
  expect_true(all(is.na(submission$Metric$metricID)))
  expect_equal(nrow(submission$DatasetTaskMetric), 2)
  expect_true(all(is.na(submission$DatasetTaskMetric$datasetTaskID)))
  expect_true(all(is.na(submission$DatasetTaskMetric$metricID)))
  expect_equal(submission$submission_links$dataset_temp_id, "SUBMISSION_DATASET_1")
  expect_equal(nrow(submission$submission_links$task), 1)
  expect_equal(nrow(submission$submission_links$evidence), 2)
  expect_equal(nrow(submission$submission_links$metric), 2)
})

testthat::test_that("prepareTrioSubmissionFiles saves dataset and evidence files", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = c("RMSE", "Pearson")
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      RMSE = MSEmetric,
      Pearson = function(evidence, predicted) stats::cor(evidence, predicted)
    )
  )
  trio$name <- "BREAST_ST"

  out_dir <- tempdir()
  prepared <- prepareTrioSubmissionFiles(
    trio = trio,
    outputDir = out_dir,
    saveData = TRUE,
    saveEvidence = TRUE,
    useExistingSource = FALSE
  )

  expect_true(prepared$dataset$prepared)
  expect_true(prepared$evidence$prepared)
  expect_true(file.exists(prepared$dataset$file))
  expect_true(file.exists(prepared$evidence$file))
  expect_equal(basename(prepared$dataset$file), "BREAST_ST_dataset.rds")
  expect_equal(basename(prepared$evidence$file), "BREAST_ST_evidence.rds")
  expect_false(is.na(prepared$dataset$md5))
  expect_false(is.na(prepared$evidence$md5))
})

testthat::test_that("collectDatasetSubmissionInfo returns dataset args in non-interactive mode", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )
  trio$name <- "BREAST_ST"
  trio$description <- "Breast cancer spatial transcriptomics dataset"

  dataset_args <- collectDatasetSubmissionInfo(
    trio = trio,
    defaults = list(
      dataType = "omics",
      dataModality = "other",
      technology = "10x Visium",
      doi = "10.1000/test",
      organism = "Homo sapiens",
      tissue = "breast",
      status = "diseased"
    )
  )

  expect_equal(dataset_args$name, "BREAST_ST")
  expect_equal(dataset_args$description, "Breast cancer spatial transcriptomics dataset")
  expect_equal(dataset_args$dataType, "omics")
  expect_equal(dataset_args$dataModality, "other")
  expect_equal(dataset_args$technology, "10x Visium")
  expect_equal(dataset_args$doi, "10.1000/test")
  expect_equal(dataset_args$organism, "Homo sapiens")
  expect_equal(dataset_args$tissue, "breast")
  expect_equal(dataset_args$status, "diseased")
})

testthat::test_that("collectTaskSubmissionInfo returns task args in non-interactive mode", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    ),
    marker_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )

  task_args <- collectTaskSubmissionInfo(
    trio = trio,
    n_tasks = 2,
    defaults = list(
      taskStage = c("downstream", "intermediate"),
      taskType = c("deconvolution", "classification"),
      taskName = c("Cell type deconvolution", "Tumour subtype prediction")
    )
  )

  expect_equal(task_args$taskStage, c("downstream", "intermediate"))
  expect_equal(task_args$taskType, c("deconvolution", "classification"))
  expect_equal(task_args$taskName, c("Cell type deconvolution", "Tumour subtype prediction"))
})

testthat::test_that("collectTaskSubmissionInfo rejects more tasks than evidence", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )

  expect_error(
    collectTaskSubmissionInfo(
      trio = trio,
      n_tasks = 2,
      defaults = list(
        taskStage = c("downstream", "intermediate"),
        taskType = c("deconvolution", "classification"),
        taskName = c("Cell type deconvolution", "Tumour subtype prediction")
      )
    ),
    "The number of tasks cannot exceed the number of supporting evidence items"
  )
})

testthat::test_that("collectEvidenceSubmissionInfo assigns evidence to tasks", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    ),
    marker_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )

  task_args <- list(
    taskStage = c("downstream", "intermediate"),
    taskType = c("deconvolution", "classification"),
    taskName = c("Cell type deconvolution", "Tumour subtype prediction")
  )

  evidence_args <- collectEvidenceSubmissionInfo(
    trio = trio,
    task_args = task_args,
    defaults = list(
      taskName = c("Cell type deconvolution", "Tumour subtype prediction"),
      evidenceType = c("manual_annotation", "reference_based")
    )
  )

  expect_equal(
    evidence_args$evidenceName,
    c("manual_annotation", "marker_annotation")
  )
  expect_equal(
    evidence_args$evidenceType,
    c("manual_annotation", "reference_based")
  )
  expect_equal(
    unname(evidence_args$evidence_task_map),
    c("Cell type deconvolution", "Tumour subtype prediction")
  )
  expect_equal(
    evidence_args$datasetTaskID,
    c("SUBMISSION_TASK_1", "SUBMISSION_TASK_2")
  )
})

testthat::test_that("collectEvidenceSubmissionInfo rejects tasks without evidence", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )

  task_args <- list(
    taskStage = c("downstream", "intermediate"),
    taskType = c("deconvolution", "clustering"),
    taskName = c("Cell type deconvolution", "Clustering")
  )

  expect_error(
    collectEvidenceSubmissionInfo(
      trio = trio,
      task_args = task_args,
      defaults = list(
        taskName = "Cell type deconvolution",
        evidenceType = "manual_annotation"
      )
    ),
    "Each task must have at least one supporting evidence assigned"
  )
})

testthat::test_that("collectMetricSubmissionInfo returns metric args in non-interactive mode", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = c("MSE", "customScore")
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      MSE = MSEmetric,
      customScore = function(evidence, predicted) mean(predicted)
    )
  )

  metric_args <- collectMetricSubmissionInfo(
    trio = trio,
    defaults = list(
      metricType = c("error_based", "task_specific")
    )
  )

  expect_equal(metric_args$metricName, c("MSE", "customScore"))
  expect_equal(unname(metric_args$metricType), c("error_based", "task_specific"))
  expect_equal(names(metric_args$metricType), c("MSE", "customScore"))
})

testthat::test_that("collectDatasetTaskMetricSubmission builds task-metric links", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = c("MSE", "customScore")
    ),
    marker_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = "MSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      MSE = MSEmetric,
      customScore = function(evidence, predicted) mean(predicted)
    )
  )

  task_args <- list(
    taskStage = c("downstream", "intermediate"),
    taskType = c("deconvolution", "classification"),
    taskName = c("Cell type deconvolution", "Tumour subtype prediction")
  )

  evidence_args <- list(
    datasetTaskID = c("SUBMISSION_TASK_1", "SUBMISSION_TASK_1"),
    evidenceName = c("manual_annotation", "marker_annotation"),
    evidenceType = c("manual_annotation", "reference_based"),
    evidence_task_map = c(
      manual_annotation = "Cell type deconvolution",
      marker_annotation = "Cell type deconvolution"
    )
  )

  task_metric_tbl <- collectDatasetTaskMetricSubmission(
    trio = trio,
    evidence_args = evidence_args,
    task_args = task_args
  )

  expect_equal(nrow(task_metric_tbl), 2)
  expect_true(all(is.na(task_metric_tbl$datasetTaskMetricID)))
  expect_equal(task_metric_tbl$datasetTaskID, c("SUBMISSION_TASK_1", "SUBMISSION_TASK_1"))
  expect_setequal(task_metric_tbl$metricID, c("MSE", "customScore"))
})

testthat::test_that("prepareTrioSubmissionFiles verifies prepared files against Figshare metadata", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  dataset_md5 <- NULL
  evidence_md5 <- NULL
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )
  original_figshare <- if (exists("figshareListFiles", envir = environment(), inherits = FALSE)) {
    get("figshareListFiles", envir = environment(), inherits = FALSE)
  } else {
    NULL
  }
  assign(
    "figshareListFiles",
    function(articleID, fileID = NULL) {
      data.frame(
        id = c("101", "102"),
        name = c("BREAST_ST_dataset.rds", "BREAST_ST_evidence.rds"),
        computed_md5 = c(dataset_md5, evidence_md5),
        stringsAsFactors = FALSE
      )
    },
    envir = environment()
  )
  withr::defer({
    if (is.null(original_figshare)) {
      rm("figshareListFiles", envir = environment())
    } else {
      assign("figshareListFiles", original_figshare, envir = environment())
    }
  })

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c("A", "B", "A"), rownames(data)),
      metrics = "RMSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(RMSE = MSEmetric)
  )
  trio$name <- "BREAST_ST"

  prepared <- prepareTrioSubmissionFiles(
    trio = trio,
    outputDir = tempdir(),
    saveData = TRUE,
    saveEvidence = TRUE,
    useExistingSource = FALSE
  )

  dataset_md5 <- prepared$dataset$md5
  evidence_md5 <- prepared$evidence$md5

  verified <- prepareTrioSubmissionFiles(
    trio = trio,
    outputDir = tempdir(),
    saveData = TRUE,
    saveEvidence = TRUE,
    useExistingSource = FALSE,
    verifyFigshare = TRUE,
    figshareUrl = "https://figshare.com/articles/dataset/BREAST_ST/123456"
  )

  expect_equal(verified$figshare$article_id, "123456")
  expect_equal(verified$figshare$dataset$source, "figshare")
  expect_equal(verified$figshare$dataset$sourceID, "123456/101")
  expect_equal(verified$figshare$evidence$sourceID, "123456/102")
  expect_true(verified$figshare$dataset$md5_ok)
  expect_true(verified$figshare$evidence$md5_ok)
})

testthat::test_that("prepareTrioSubmissionMetrics classifies internal and custom metrics", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = c("MSE", "customScore")
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      MSE = MSEmetric,
      customScore = function(evidence, predicted) mean(predicted)
    )
  )
  trio$name <- "BREAST_ST"

  metric_info <- prepareTrioSubmissionMetrics(
    trio = trio,
    uploadCustom = FALSE
  )

  expect_equal(nrow(metric_info$Metric), 2)
  expect_equal(
    metric_info$Metric$metricSourceType[metric_info$Metric$metricName == "MSE"],
    "internal"
  )
  expect_equal(
    metric_info$Metric$wrapper_r[metric_info$Metric$metricName == "MSE"],
    "MSEmetric"
  )
  expect_equal(
    metric_info$Metric$metricSourceType[metric_info$Metric$metricName == "customScore"],
    "gist"
  )
  expect_equal(
    metric_info$Metric$wrapper_r[metric_info$Metric$metricName == "customScore"],
    "customScore"
  )
  expect_true(all(is.na(metric_info$Metric$metricType)))
  expect_true(is.na(
    metric_info$Metric$gist_url[metric_info$Metric$metricName == "customScore"]
  ))
  expect_true(any(grepl("^customScore <-", metric_info$custom_metric_lines)))
})

testthat::test_that("prepareTrioSubmissionMetrics uploads custom metrics to gist", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  original_creat <- if (exists("creatGist", envir = environment(), inherits = FALSE)) {
    get("creatGist", envir = environment(), inherits = FALSE)
  } else {
    NULL
  }
  assign(
    "creatGist",
    function(content, filename, description, public = TRUE, pat = NULL) {
      list(
        html_url = "https://gist.github.com/example/custom-metric",
        content = content,
        filename = filename,
        description = description,
        public = public,
        pat = pat
      )
    },
    envir = environment()
  )
  withr::defer({
    if (is.null(original_creat)) {
      rm("creatGist", envir = environment())
    } else {
      assign("creatGist", original_creat, envir = environment())
    }
  })

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = "customScore"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(
      customScore = function(evidence, predicted) mean(predicted)
    )
  )
  trio$name <- "BREAST_ST"

  metric_info <- prepareTrioSubmissionMetrics(
    trio = trio,
    uploadCustom = TRUE,
    githubPat = "test_pat"
  )

  expect_equal(
    metric_info$gist$html_url,
    "https://gist.github.com/example/custom-metric"
  )
  expect_equal(
    metric_info$Metric$gist_url,
    "https://gist.github.com/example/custom-metric"
  )
  expect_equal(metric_info$Metric$wrapper_r, "customScore")
})

testthat::test_that("prepareTrioSubmissionBundle returns a pre-submit bundle", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  dataset_md5 <- NULL
  evidence_md5 <- NULL
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )
  original_figshare <- if (exists("figshareListFiles", envir = environment(), inherits = FALSE)) {
    get("figshareListFiles", envir = environment(), inherits = FALSE)
  } else {
    NULL
  }
  assign(
    "figshareListFiles",
    function(articleID, fileID = NULL) {
      data.frame(
        id = c("101", "102"),
        name = c("BREAST_ST_dataset.rds", "BREAST_ST_evidence.rds"),
        computed_md5 = c(dataset_md5, evidence_md5),
        stringsAsFactors = FALSE
      )
    },
    envir = environment()
  )
  withr::defer({
    if (is.null(original_figshare)) {
      rm("figshareListFiles", envir = environment())
    } else {
      assign("figshareListFiles", original_figshare, envir = environment())
    }
  })

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = "MSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(MSE = MSEmetric)
  )
  trio$name <- "BREAST_ST"

  prepared <- prepareTrioSubmissionFiles(
    trio = trio,
    outputDir = tempdir(),
    saveData = TRUE,
    saveEvidence = TRUE,
    useExistingSource = FALSE
  )
  dataset_md5 <- prepared$dataset$md5
  evidence_md5 <- prepared$evidence$md5

  bundle <- prepareTrioSubmissionBundle(
    trio = trio,
    dataset_args = list(
      name = "BREAST_ST",
      dataType = "omics",
      dataModality = "other",
      technology = "10x Visium",
      description = "Breast cancer spatial transcriptomics dataset",
      doi = "10.1000/test",
      tissue = "breast",
      status = "diseased"
    ),
    task_args = list(
      taskStage = "downstream",
      taskType = "deconvolution",
      taskName = "Cell type deconvolution"
    ),
    evidence_task_map = c(
      manual_annotation = "Cell type deconvolution"
    ),
    prepare_files = TRUE,
    file_args = list(
      outputDir = tempdir(),
      saveData = TRUE,
      saveEvidence = TRUE,
      useExistingSource = FALSE,
      verifyFigshare = TRUE,
      figshareUrl = "https://figshare.com/articles/dataset/BREAST_ST/123456"
    ),
    build_payload = TRUE,
    build_json = TRUE
  )

  expect_true(is.list(bundle))
  expect_false(is.null(bundle$submission))
  expect_false(is.null(bundle$files))
  expect_false(is.null(bundle$payload))
  expect_false(is.null(bundle$json))
  expect_equal(bundle$submission$Dataset$source, "figshare")
  expect_equal(bundle$submission$Dataset$datasourceID, "123456/101")
  expect_equal(bundle$submission$DatasetEvidence$evidenceSourceID, "123456/102")
})

testthat::test_that("writeSubmission builds a full pre-submit object without topic", {
  sys.source(
    submission_dataset_source,
    envir = environment()
  )
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  rownames(data) <- c("spot_1", "spot_2", "spot_3")

  evidence <- list(
    manual_annotation = list(
      evidence = setNames(c(1, 2, 3), rownames(data)),
      metrics = "MSE"
    )
  )

  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data,
    evidence = evidence,
    metrics = list(MSE = MSEmetric)
  )
  trio$name <- "BREAST_ST"
  trio$description <- "Breast cancer spatial transcriptomics dataset"

  result <- writeSubmission(
    trio = trio,
    n_tasks = 1,
    dataset_defaults = list(
      name = "BREAST_ST",
      dataType = "omics",
      dataModality = "other",
      technology = "10x Visium",
      description = "Breast cancer spatial transcriptomics dataset",
      doi = "10.1000/test",
      organism = "Homo sapiens",
      tissue = "breast",
      status = "diseased"
    ),
    task_defaults = list(
      taskStage = "downstream",
      taskType = "deconvolution",
      taskName = "Cell type deconvolution"
    ),
    evidence_defaults = list(
      taskName = "Cell type deconvolution",
      evidenceType = "manual_annotation"
    ),
    metric_defaults = list(
      metricType = "error_based"
    ),
    prepare_files = FALSE,
    upload_custom_metrics = FALSE,
    build_payload = TRUE,
    build_json = TRUE
  )

  expect_true(is.list(result))
  expect_false("topic" %in% names(result$submission$DatasetTask))
  expect_equal(result$submission$DatasetTask$taskStage, "downstream")
  expect_equal(result$submission$DatasetTask$taskName, "Cell type deconvolution")
  expect_equal(result$submission$DatasetEvidence$evidenceType, "manual_annotation")
  expect_equal(result$submission$Metric$metricType, "error_based")
  expect_false(is.null(result$payload))
  expect_false(is.null(result$json))
})
