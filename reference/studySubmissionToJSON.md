# Convert a Study submission to JSON

Convert a Study submission to JSON

## Usage

``` r
studySubmissionToJSON(submission, pretty = TRUE)
```

## Arguments

- submission:

  A submission object returned by
  [`buildStudySubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildStudySubmission.md).

- pretty:

  Whether to pretty-print the JSON. Defaults to `TRUE`.

## Value

A JSON string.

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
json <- studySubmissionToJSON(submission)
substr(json, 1, 20)
#> {
#>   "payload": {
#>     
```
