# Convert a Study submission to payload structure

Convert a Study submission to payload structure

## Usage

``` r
buildStudySubmissionPayload(submission)
```

## Arguments

- submission:

  A submission object returned by
  [`buildStudySubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildStudySubmission.md).

## Value

A named list with a top-level `payload` entry.

## Examples

``` r
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
payload <- buildStudySubmissionPayload(submission)
names(payload)
#> [1] "payload"
```
