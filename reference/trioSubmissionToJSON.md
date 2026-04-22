# Convert a combined Trio submission to JSON

Convert a combined Trio submission to JSON

## Usage

``` r
trioSubmissionToJSON(submission, pretty = TRUE)
```

## Arguments

- submission:

  A submission object returned by
  [`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md).

- pretty:

  Whether to pretty-print the JSON. Defaults to `TRUE`.

## Value

A JSON string.

## Examples

``` r
submission <- BenchHub:::private_example_trio_submission()
#> Error: object 'private_example_trio_submission' not found
json <- trioSubmissionToJSON(submission)
#> Error: object 'submission' not found
substr(json, 1, 20)
#> Error: object 'json' not found
```
