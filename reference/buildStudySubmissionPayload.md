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
submission <- BenchHub:::private_example_study_submission()
#> Error: object 'private_example_study_submission' not found
payload <- buildStudySubmissionPayload(submission)
#> Error: object 'submission' not found
names(payload)
#> Error: object 'payload' not found
```
