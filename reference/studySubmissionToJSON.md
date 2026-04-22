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
submission <- BenchHub:::private_example_study_submission()
#> Error: object 'private_example_study_submission' not found
json <- studySubmissionToJSON(submission)
#> Error: object 'submission' not found
substr(json, 1, 20)
#> Error: object 'json' not found
```
