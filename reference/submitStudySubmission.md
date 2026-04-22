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
submission <- BenchHub:::private_example_study_submission()
submitStudySubmission(
  submission,
  url = "https://script.google.com/macros/s/example/exec",
  submittedBy = "researcher@example.org"
)
} # }
```
