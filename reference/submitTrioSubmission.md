# Submit a Trio submission payload to Google Apps Script

This helper posts the Trio submission payload to a Google Apps Script
endpoint using the same redirect-tolerant approach as the prototype
script.

## Usage

``` r
submitTrioSubmission(submission, url, submittedBy, submittedType = "Trio")
```

## Arguments

- submission:

  A submission object returned by
  [`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md).

- url:

  Google Apps Script endpoint URL.

- submittedBy:

  Submitter email or identifier.

- submittedType:

  Submission type label. Defaults to `"Trio"`.

## Value

A list containing request status information and response text.
