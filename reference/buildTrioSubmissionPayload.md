# Convert a combined Trio submission to payload structure

This helper converts the output of
[`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md)
into the nested list structure used by the JSON payload in the prototype
submission script.

## Usage

``` r
buildTrioSubmissionPayload(submission)
```

## Arguments

- submission:

  A submission object returned by
  [`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md).

## Value

A named list with a top-level `payload` entry ready for
`jsonlite::toJSON(..., auto_unbox = TRUE)`.
