# Prepare a Study submission bundle

Prepare a Study submission bundle

## Usage

``` r
prepareStudySubmission(
  study,
  datasetIDs = NULL,
  available_datasets = NULL,
  existing_studies = NULL,
  defaults = list(),
  protocolGist = NULL,
  mappingFunctions = NULL,
  protocolFile = NULL,
  mappingFunctionsFile = NULL,
  uploadProtocol = FALSE,
  uploadMappingFunctions = FALSE,
  githubPat = Sys.getenv("GITHUB_PAT"),
  gistPublic = TRUE,
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

- study:

  A `BenchmarkStudy` object.

- datasetIDs:

  Optional character vector of existing dataset IDs to link.

- available_datasets:

  Optional data frame of Dataset rows for selection.

- existing_studies:

  Optional data frame of Study rows.

- defaults:

  Optional named list for
  [`collectStudySubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectStudySubmissionInfo.md).

- protocolGist:

  Optional protocol gist URL.

- mappingFunctions:

  Optional mapping functions gist URL.

- protocolFile:

  Optional local file path to upload as a protocol gist.

- mappingFunctionsFile:

  Optional local file path to upload as a mapping functions gist.

- uploadProtocol:

  Logical; whether to upload `protocolFile` to a gist.

- uploadMappingFunctions:

  Logical; whether to upload `mappingFunctionsFile` to a gist.

- githubPat:

  Optional GitHub personal access token.

- gistPublic:

  Logical; whether uploaded gists should be public.

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

A named list containing the Study submission bundle.

## Examples

``` r
study <- BenchmarkStudy$new(name = "example_study")
study$description <- "A small example benchmark study."
available_datasets <- data.frame(
  datasetID = "dataset_001",
  name = "example_dataset",
  stringsAsFactors = FALSE
)
existing_studies <- data.frame(
  studyID = character(0),
  studyName = character(0),
  version = character(0),
  description = character(0),
  type = character(0),
  protocolGist = character(0),
  mappingFunctions = character(0),
  stringsAsFactors = FALSE
)
result <- prepareStudySubmission(
  study,
  datasetIDs = "dataset_001",
  available_datasets = available_datasets,
  existing_studies = existing_studies,
  build_json = FALSE,
  review = FALSE
)
names(result)
#> [1] "study_info" "submission" "payload"   
```
