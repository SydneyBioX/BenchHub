testthat::test_that("buildStudySubmission creates original Study submission rows", {
  study <- BenchmarkStudy$new(name = "Spatial Study")
  study$description <- "A benchmark study over existing trios."

  submission <- buildStudySubmission(
    study = study,
    datasetIDs = c("D001", "D002"),
    existing_studies = data.frame(
      studyID = character(0),
      studyName = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(submission$Study$studyName, "Spatial Study")
  expect_equal(submission$Study$version, "0.0.1")
  expect_equal(submission$Study$type, "original")
  expect_true(all(is.na(submission$Study$studyID)))
  expect_equal(submission$StudyDataset$datasetID, c("D001", "D002"))
  expect_true(all(is.na(submission$StudyDataset$studyDatasetID)))
  expect_true(all(is.na(submission$StudyDataset$studyID)))
})

testthat::test_that("buildStudySubmission increments version for updates", {
  study <- BenchmarkStudy$new(name = "Spatial Study")
  study$description <- "Updated benchmark study."

  submission <- buildStudySubmission(
    study = study,
    datasetIDs = "D005",
    existing_studies = data.frame(
      studyID = c("ST001", "ST002"),
      studyName = c("Spatial Study", "Spatial Study"),
      version = c("0.0.1", "0.0.2"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(submission$Study$type, "update")
  expect_equal(submission$Study$version, "0.0.3")
})

testthat::test_that("buildStudySubmissionPayload returns Study payload shape", {
  study <- BenchmarkStudy$new(name = "Spatial Study")
  study$description <- "Payload study."

  submission <- buildStudySubmission(
    study = study,
    datasetIDs = c("D001", "D002"),
    existing_studies = data.frame(
      studyID = character(0),
      studyName = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    protocolGist = "",
    mappingFunctions = ""
  )

  payload <- buildStudySubmissionPayload(submission)

  expect_true("payload" %in% names(payload))
  expect_equal(payload$payload$Study$studyName, "Spatial Study")
  expect_equal(length(payload$payload$StudyDataset), 2)
  expect_equal(payload$payload$StudyDataset[[1]]$datasetID, "D001")
})

testthat::test_that("collectStudySubmissionInfo infers dataset IDs from trio names", {
  trio <- Trio$new(
    datasetID = "BREAST_ST",
    data = data.frame(x = 1:3),
    evidence = list(),
    metrics = list()
  )
  trio$name <- "Dataset A"

  study <- BenchmarkStudy$new(name = "Spatial Study", trios = list(trio))
  study$description <- "Study with inferred datasets."

  info <- collectStudySubmissionInfo(
    study = study,
    available_datasets = data.frame(
      datasetID = c("D001", "D002"),
      name = c("Dataset A", "Dataset B"),
      stringsAsFactors = FALSE
    ),
    existing_studies = data.frame(
      studyID = character(0),
      studyName = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    defaults = list(
      protocolGist = "",
      mappingFunctions = ""
    )
  )

  expect_equal(info$datasetIDs, "D001")
  expect_equal(info$type, "original")
  expect_equal(info$version, "0.0.1")
})

testthat::test_that("prepareStudySubmission builds payload and JSON", {
  study <- BenchmarkStudy$new(name = "Prepared Study")
  study$description <- "Prepared study description."

  result <- prepareStudySubmission(
    study = study,
    datasetIDs = c("D010", "D011"),
    available_datasets = data.frame(
      datasetID = c("D010", "D011"),
      name = c("Dataset X", "Dataset Y"),
      stringsAsFactors = FALSE
    ),
    existing_studies = data.frame(
      studyID = character(0),
      studyName = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    defaults = list(
      protocolGist = "",
      mappingFunctions = ""
    ),
    review = FALSE,
    submit = FALSE
  )

  expect_true("submission" %in% names(result))
  expect_true("payload" %in% names(result))
  expect_true("json" %in% names(result))
  expect_equal(result$submission$Study$studyName, "Prepared Study")
  expect_equal(result$submission$StudyDataset$datasetID, c("D010", "D011"))
})

testthat::test_that("submitStudySubmission accepts the default submittedType", {
  study <- BenchmarkStudy$new(name = "Submit Study")
  study$description <- "Submit study description."

  submission <- buildStudySubmission(
    study = study,
    datasetIDs = "D010",
    existing_studies = data.frame(
      studyID = character(0),
      studyName = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    has_internet = function() TRUE,
    .package = "curl"
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "mock_req"),
    req_method = function(req, method) req,
    req_headers = function(req, ...) req,
    req_body_json = function(req, body, auto_unbox = TRUE) req,
    req_options = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) structure(list(), class = "mock_resp"),
    resp_status = function(resp) 200L,
    resp_headers = function(resp) list(),
    resp_body_string = function(resp) "{\"ok\":true}",
    .package = "httr2"
  )

  expect_no_error(
    submitStudySubmission(
      submission = submission,
      url = "https://example.com",
      submittedBy = "tester@example.com"
    )
  )
})

testthat::test_that("getSubmissionStudy and getSubmissionStudyDatasets use studyID", {
  studies <- data.frame(
    studyID = c("ST001", "ST002"),
    studyName = c("SpatialSimBench", "SpatialSimBench"),
    version = c("0.0.1", "0.0.2"),
    description = c("First", "Second"),
    type = c("original", "update"),
    protocolGist = c(NA_character_, "https://example.org/protocol"),
    mappingFunctions = c(NA_character_, "https://example.org/mapping"),
    stringsAsFactors = FALSE
  )
  study_datasets <- data.frame(
    studyDatasetID = c("SD001", "SD002", "SD003"),
    studyID = c("ST001", "ST002", "ST002"),
    datasetID = c("D001", "D002", "D003"),
    stringsAsFactors = FALSE
  )

  study_row <- getSubmissionStudy("ST002", studies = studies)
  linked_rows <- getSubmissionStudyDatasets("ST002", study_datasets = study_datasets)

  expect_equal(study_row$studyID, "ST002")
  expect_equal(study_row$version, "0.0.2")
  expect_equal(linked_rows$datasetID, c("D002", "D003"))
})

testthat::test_that("prepareStudyUpdateSubmission builds a new update version from studyID", {
  studies <- data.frame(
    studyID = c("ST001", "ST002"),
    studyName = c("SpatialSimBench", "SpatialSimBench"),
    version = c("0.0.1", "0.0.2"),
    description = c("First version", "Latest version"),
    type = c("original", "update"),
    protocolGist = c(NA_character_, "https://example.org/protocol"),
    mappingFunctions = c(NA_character_, "https://example.org/mapping"),
    stringsAsFactors = FALSE
  )
  study_datasets <- data.frame(
    studyDatasetID = c("SD001", "SD002", "SD003"),
    studyID = c("ST001", "ST002", "ST002"),
    datasetID = c("D001", "D002", "D003"),
    stringsAsFactors = FALSE
  )

  result <- prepareStudyUpdateSubmission(
    studyID = "ST002",
    studies = studies,
    study_datasets = study_datasets,
    review = FALSE,
    submit = FALSE
  )

  expect_equal(result$submission$Study$studyName, "SpatialSimBench")
  expect_equal(result$submission$Study$type, "update")
  expect_equal(result$submission$Study$version, "0.0.3")
  expect_equal(result$submission$Study$description, "Latest version")
  expect_equal(result$submission$StudyDataset$datasetID, c("D002", "D003"))
})

testthat::test_that("downloadSubmissionStudy loads a study by studyID and linked trios", {
  studies <- data.frame(
    studyID = c("ST001", "ST002"),
    studyName = c("SpatialSimBench", "SpatialSimBench"),
    version = c("0.0.1", "0.0.2"),
    description = c("First version", "Latest version"),
    mappingFunctions = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  study_datasets <- data.frame(
    studyDatasetID = c("SD001", "SD002", "SD003"),
    studyID = c("ST001", "ST002", "ST002"),
    datasetID = c("D001", "D002", "D003"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    has_internet = function() TRUE,
    .package = "curl"
  )
  testthat::local_mocked_bindings(
    private_read_submission_database_sheet = function(ss, sheet) {
      if (identical(sheet, "Study")) {
        return(studies)
      }
      if (identical(sheet, "StudyDataset")) {
        return(study_datasets)
      }
      stop("unexpected sheet")
    },
    downloadSubmissionTrio = function(datasetID, ss, cachePath) {
      trio <- Trio$new(
        datasetID = datasetID,
        data = data.frame(x = 1:3),
        evidence = list(),
        metrics = list()
      )
      trio$name <- paste("dataset", datasetID)
      trio
    },
    .env = environment()
  )

  study <- downloadSubmissionStudy(studyID = "ST002")

  expect_s3_class(study, "BenchmarkStudy")
  expect_equal(study$name, "SpatialSimBench")
  expect_equal(study$version, "0.0.2")
  expect_equal(study$description, "Latest version")
  expect_length(study$trios, 2)
  expect_equal(vapply(study$trios, function(x) x$name, character(1)), c("dataset D002", "dataset D003"))
})
