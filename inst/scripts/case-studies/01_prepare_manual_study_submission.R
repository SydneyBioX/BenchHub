# Manually prepare one BenchmarkStudy submission.
#
# This example uses the new Study submission helpers without modifying the
# older BenchmarkStudy workflow. Replace the dataset IDs, study metadata, and
# Apps Script URL with your real values before submitting.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(BenchHub)
}
source("R/BenchmarkStudy.R")
pkgload::load_all(".")

# Optional: inspect the datasets already available for Study linking.
available_datasets <- listSubmissionStudyDatasets()
print(utils::head(available_datasets[, c("datasetID", "name"), drop = FALSE], 10))

# Recommended interactive path:
# bundle <- interactivePrepareStudySubmission(study)

# Replace these with the dataset IDs you want to link into this Study.
dataset_ids <- c("D001", "D002")

# Create a small BenchmarkStudy object.
study <- BenchmarkStudy$new(name = "Example Benchmark Study")
study$description <- paste(
  "Small test Study submission built from existing Trio datasets.",
  "Update the study name, description, and linked dataset IDs before real use."
)

# Optional: add Trio objects so dataset IDs can be inferred automatically when
# the Trio names match dataset names already in the submission sheet.
# study$addTrio(trio1)
# study$addTrio(trio2)

# Optional gist URLs. Leave empty for the small first version.
protocol_gist <- ""
mapping_functions_gist <- ""

# Optional: upload local files to gist instead of supplying existing gist URLs.
upload_protocol <- FALSE
protocol_file <- NULL
upload_mapping_functions <- FALSE
mapping_functions_file <- NULL

# Optional submit settings.
submission_url <- "https://script.google.com/macros/s/AKfycbwV4PbJBvkGcuSJhmoByfIliAZsy4if56-s1fvkGUJSqpjcrZlqdb-ZoMZr1jjx1Hu0/exec"
submitted_by <- "user1@example.org"

bundle <- prepareStudySubmission(
  study = study,
  datasetIDs = dataset_ids,
  protocolGist = protocol_gist,
  mappingFunctions = mapping_functions_gist,
  uploadProtocol = upload_protocol,
  protocolFile = protocol_file,
  uploadMappingFunctions = upload_mapping_functions,
  mappingFunctionsFile = mapping_functions_file,
  githubPat = Sys.getenv("GITHUB_PAT"),
  review = TRUE,
  submit = FALSE,
  url = submission_url,
  submittedBy = submitted_by
)

response <- submitStudySubmission(
  submission = bundle$submission,
  url = submission_url,
  submittedBy = submitted_by
)

submitStudySubmission
