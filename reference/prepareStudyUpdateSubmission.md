# Prepare an update submission from an existing Study version

Prepare an update submission from an existing Study version

## Usage

``` r
prepareStudyUpdateSubmission(
  studyID,
  study = NULL,
  datasetIDs = NULL,
  description = NULL,
  protocolGist = NULL,
  mappingFunctions = NULL,
  studies = NULL,
  study_datasets = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  build_payload = TRUE,
  build_json = TRUE,
  review = TRUE,
  submit = FALSE,
  url = submission_webapp_url,
  submittedBy = NULL
)
```

## Arguments

- studyID:

  Existing Study identifier to use as the update baseline.

- study:

  Optional `BenchmarkStudy` object. When omitted, a new one is created
  from the existing Study row.

- datasetIDs:

  Optional replacement dataset IDs. Defaults to the linked datasets from
  the baseline Study.

- description:

  Optional replacement description.

- protocolGist:

  Optional replacement protocol gist URL.

- mappingFunctions:

  Optional replacement mapping functions gist URL.

- studies:

  Optional data frame of Study rows.

- study_datasets:

  Optional data frame of StudyDataset rows.

- ss:

  Submission spreadsheet ID or URL.

- build_payload:

  Whether to include the nested payload. Defaults to `TRUE`.

- build_json:

  Whether to include JSON output. Defaults to `TRUE`.

- review:

  Whether to print a short submission summary. Defaults to `TRUE`.

- submit:

  Whether to submit immediately. Defaults to `FALSE`.

- url:

  Google Apps Script endpoint URL. Defaults to the package submission
  web app.

- submittedBy:

  Submitter email or identifier.

## Value

A named list containing the updated Study submission bundle.

## Examples

``` r
studies <- data.frame(
  studyID = "study_001",
  studyName = "example_study",
  version = "0.0.1",
  description = "A small example benchmark study.",
  type = "original",
  protocolGist = "",
  mappingFunctions = "",
  stringsAsFactors = FALSE
)
study_datasets <- data.frame(
  studyDatasetID = "study_dataset_001",
  studyID = "study_001",
  datasetID = "dataset_001",
  stringsAsFactors = FALSE
)
result <- prepareStudyUpdateSubmission(
  studyID = "study_001",
  studies = studies,
  study_datasets = study_datasets,
  build_json = FALSE,
  review = FALSE
)
names(result)
#> [1] "study_info" "submission" "payload"   
```
