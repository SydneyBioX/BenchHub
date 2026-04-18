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
