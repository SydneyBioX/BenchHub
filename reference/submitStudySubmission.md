# Submit a Study submission payload to Google Apps Script

Submit a Study submission payload to Google Apps Script

## Usage

``` r
submitStudySubmission(submission, url, submittedBy)
```

## Arguments

- submission:

  A submission object returned by
  [`buildStudySubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildStudySubmission.md).

- url:

  Google Apps Script endpoint URL.

- submittedBy:

  Submitter email or identifier.

## Value

A list containing request status information and response text.

## Examples

``` r
if (FALSE) { # \dontrun{
study <- BenchmarkStudy$new(name = "example_study")
study$description <- "A small example benchmark study."
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
submission <- buildStudySubmission(
  study,
  datasetIDs = "dataset_001",
  existing_studies = existing_studies
)
submitStudySubmission(
  submission,
  url = "https://script.google.com/macros/s/example/exec",
  submittedBy = "researcher@example.org"
)
} # }
```
