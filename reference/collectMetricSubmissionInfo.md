# Collect Metric submission metadata interactively

This helper gathers user-supplied metric classification for each metric
in a `Trio`. The metric names and wrappers still come from the Trio
object and the package helper logic.

## Usage

``` r
collectMetricSubmissionInfo(trio, defaults = list())
```

## Arguments

- trio:

  A `Trio` object.

- defaults:

  Optional named list with entry `metricType`. It may be length 1 or
  length equal to the number of metrics in the Trio.

## Value

A named list ready to merge into the `Metric` submission table.

## Examples

``` r
trio <- BenchHub:::private_example_submission_trio()
#> Error: object 'private_example_submission_trio' not found
collectMetricSubmissionInfo(
  trio,
  defaults = list(metricType = "label_based")
)
#> Error: object 'trio' not found
```
